{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Network.GCM
    ( Message
    , send
    , Config
    , defConf
    , Payload
    , GcmStatus
    ) where
import           Prelude hiding (concat, length)
import           Control.Lens           hiding ((.=))
import           Control.Retry (retrying, RetryStatus, constantDelay, limitRetries)
import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, decode)
import           Data.Aeson.Types (Value(Object), (.:), (.:?), (.=), (.!=), object)
import           Data.Maybe (isNothing, Maybe, fromJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Vector (Vector, length, fromList)
import           Data.ByteString (ByteString, concat)
import           Network.Wreq (postWith, defaults, header, Options, responseBody, checkStatus)
import           Control.Monad.Trans.State (StateT, evalStateT, get, put)
import           Control.Monad.IO.Class (liftIO)

type Payload = (Text, Text)
instance ToJSON Payload where
    toJSON (k, v) = object [
      k .= v
      ]

data Message a = Message {
  _registrationIDs       :: Vector String,
  _collapseKey           :: Maybe String,
  _data                  :: a,
  _delayWhileIdle        :: Maybe Bool,
  _ttl                   :: Int,
  _restrictedPackageName :: Maybe String,
  _dryRun                :: Maybe Bool
  }

instance ToJSON a => ToJSON (Message a) where
  toJSON (Message r ck d dwi t rpn dr) = object [
     "registration_ids" .= r
     , "collapse_key" .= ck
     , "data" .= d
     , "delay_while_idle" .= dwi
     , "time_to_live" .= t
     , "restricted_package_name" .= rpn
     , "dry_run" .= dr
     ]

data Response = Response {
  _multicastId  :: Integer,
  _success      :: Int,
  _failure      :: Int,
  _canonicalIds :: Int,
  _results      :: [Result]
  }

instance FromJSON Response where
  parseJSON (Object v) = Response <$>
                                v .: "multicast_id" <*>
                                v .: "success" <*>
                                v .: "failure" <*>
                                v .: "canonical_ids" <*>
                                v .: "results"

data Result = Result {
  _messageId      :: Maybe String,
  _registrationId :: Maybe String,
  _error          :: Maybe String
  }

instance FromJSON Result where
  parseJSON (Object v) = Result <$>
                               v .:? "message_id" .!= Nothing <*>
                               v .:? "registration_id" .!= Nothing <*>
                               v .:? "error" .!= Nothing

data Config = Config {
  _key     :: ByteString,
  _numRetry :: Int
  }

data GcmStatus = GcmSuccess Response | GcmError String | GcmJsonError | GcmFailed [String]

--type GcmState a = StateT (Message a) IO GcmStatus

defConf = Config "" 0

gcmSendEndpoint = "https://android.googleapis.com/gcm/send"
backoffInitialDelay = 1000
maxBackoffDelay = 1024000

send :: ToJSON a => Config -> Message a -> IO GcmStatus
send cfg msg = do
  ok <- chkMsg msg
  if ok then evalStateT (compute cfg) msg
  else return $ GcmError "Test"

compute :: ToJSON a => Config -> StateT (Message a) IO GcmStatus
compute cfg = do
    let opts = defaults & header "Authorization" .~ [concat ["key=", _key cfg]]
                        & header "Content-Type" .~ ["application/json"]
                        & checkStatus .~ Just (\_ _ _ -> Nothing)
        retryPolicy = constantDelay 20000 <> limitRetries (_numRetry cfg)
    retrying retryPolicy cond $ \_ -> do
        msg' <- get
        liftIO $ send' opts msg'

cond :: RetryStatus -> GcmStatus -> StateT (Message a) IO Bool
cond _ (GcmSuccess r) = return False
cond _ (GcmFailed ids) = do
    origMsg <- get
    put origMsg { _registrationIDs = fromList ids }
    return True
cond _ _ = return True

send' :: ToJSON a => Options -> Message a -> IO GcmStatus
send' opts msg = do
  r <- postWith opts gcmSendEndpoint (toJSON msg)
  let body = r ^. responseBody
  maybeToStatus $ decode body

maybeToStatus :: Maybe Response -> IO GcmStatus
maybeToStatus Nothing = return GcmJsonError
maybeToStatus (Just r@(Response _ _ f _ rs))
    | f == 0 = return $ GcmSuccess r
    | otherwise = return $ GcmFailed $ failedIds rs

failedIds rs = map (fromJust . _registrationId) [i | i <- rs, fromJust (_error i) == "Unavailable"]

chkMsg (Message v _ _ _ t _ _)
    | length v == 0 = return False
    | length v > 1000 = return False
    | t < 0 = return False
    | t > 2419200 = return False
    | otherwise = return True

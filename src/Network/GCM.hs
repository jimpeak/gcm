{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Network.GCM
    ( Message
    , send
    , Config
    , defConf
    , Payload
    , GcmStatus
    ) where

import           Control.Lens              hiding ((.=))
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State (StateT, evalStateT, get, put)
import           Control.Retry             (RetryStatus, constantDelay,
                                            limitRetries, retrying)
import           Data.Aeson                (FromJSON, ToJSON, decode, parseJSON,
                                            toJSON)
import           Data.Aeson.Types          (Value (Object), object, (.!=), (.:),
                                            (.:?), (.=))
import           Data.ByteString           (ByteString, concat)
import           Data.Maybe                (Maybe, fromJust, isNothing)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import           Data.Vector               (Vector, fromList, length)
import           Network.Wreq              (Options, checkStatus, defaults,
                                            header, postWith, responseBody)
import           Prelude                   hiding (concat, length)

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

instance Show Response where
    show (Response m s f c r) = ""

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
    _key      :: ByteString,
    _numRetry :: Int
    }

data GcmStatus = GcmSuccess Response | GcmError String | GcmJsonError | GcmFailed [String]

instance Show GcmStatus where
    show (GcmSuccess r) = show r
    show (GcmError e) = e
    show GcmJsonError = "Error in JSON format submitted to google's servers. Probably caused by a bad registration ID."
    show (GcmFailed s) = "Delivery failed for registration ids " ++ unwords s

defConf = Config "" 0

gcmSendEndpoint = "https://android.googleapis.com/gcm/send"

send :: ToJSON a => Config -> Message a -> IO GcmStatus
send cfg msg = do
    let ok = chkMsg msg
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
  return $ maybeToStatus $ decode body

maybeToStatus :: Maybe Response -> GcmStatus
maybeToStatus Nothing = GcmJsonError
maybeToStatus (Just r@(Response _ _ f _ rs))
    | f == 0 = GcmSuccess r
    | otherwise = GcmFailed $ failedIds rs

failedIds :: [Result] -> [String]
failedIds = map (fromJust . _registrationId) . filter (\i -> fromJust (_error i) == "Unavailable")

chkMsg :: Message a -> Bool
chkMsg (Message v _ _ _ t _ _)
    | length v == 0 = False
    | length v > 1000 = False
    | t < 0 = False
    | t > 2419200 = False
    | otherwise = True

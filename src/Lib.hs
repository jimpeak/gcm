{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Lib
    ( Message
    ) where
import           Prelude hiding (concat)
import           Control.Lens           hiding ((.=))
import           Control.Retry (retrying)
import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, decode)
import           Data.Aeson.Types (Value(Object), (.:), (.=), object)
import           Data.Default.Class (def)
import           Data.Maybe (isNothing, Maybe)
import           Data.Text (Text)
import           Data.ByteString (ByteString, concat)
import           Network.Wreq (postWith, defaults, header, Options, responseBody)

type Pair = (Text, Text)
instance ToJSON Pair where
    toJSON (k, v) = object [
      k .= v
      ]

data Message a = Message {
  _registrationIDs       :: [String],
  _collapseKey           :: String,
  _data                  :: a,
  _delayWhileIdle        :: Bool,
  _ttl                   :: Int,
  _restrictedPackageName :: String,
  _dryRun                :: Bool
  }

instance ToJSON a => ToJSON (Message a) where
  toJSON (Message r ck d dwi t rpn dr) = object [
     "registration_ids" .= r
     , "data" .= d
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
  _messageId      :: String,
  _registrationId :: String,
  _error          :: String
  }

instance FromJSON Result where
  parseJSON (Object v) = Result <$>
                               v .: "message_id" <*>
                               v .: "registration_id" <*>
                               v .: "error"

data Config = Config {
  _key     :: ByteString,
  _noRetry :: Int
  }

defConf = Config "" 0

gcmSendEndpoint = "https://android.googleapis.com/gcm/send"
backoffInitialDelay = 1000
maxBackoffDelay = 1024000

send :: ToJSON a => Config -> Message a -> IO (Maybe Response)
send cfg msg = do
  let opts = defaults & header "Authorization" .~ [concat ["key=", _key cfg]]
                      & header "Content-Type" .~ ["application/json"]
  retrying def (const $ return . isNothing) (\_ -> send' opts msg)

send' :: ToJSON a => Options -> Message a -> IO (Maybe Response)
send' opts msg = do
  r <- postWith opts gcmSendEndpoint (toJSON msg)
  let body = r ^. responseBody
  return $ decode body

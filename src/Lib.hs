{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( Message
    ) where

import           Control.Lens           hiding ((.=))
import           Control.Retry (retrying)
import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, decode)
import           Data.Aeson.Types (Value(Object), Pair, (.:), (.=), object)
import           Data.Default.Class (def)
import           Data.Maybe (isNothing, Maybe)
import           Data.Text (Text)
import           Network.Wreq (postWith, defaults, param, Options, responseBody)

data Message = Message {
  _registrationIDs       :: [String],
  _collapseKey           :: String,
  _data                  :: [Pair],
  _delayWhileIdle        :: Bool,
  _ttl                   :: Int,
  _restrictedPackageName :: String,
  _dryRun                :: Bool
  }

instance ToJSON Message where
  toJSON (Message r ck d dwi t rpn dr) = object [
     "registration_ids" .= r
     , "collapse_key" .= ck
     , "data" .= d
     , "delay_while_idle" .= dwi
     , "time_to_live" .= t
     , "restricted_package_name" .= rpn
     , "dry_ryn" .= dr
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
  _key     :: Text,
  _noRetry :: Int
  }

defConf = Config "" 0

gcmSendEndpoint = "https://android.googleapis.com/gcm/send"
backoffInitialDelay = 1000
maxBackoffDelay = 1024000

send :: Config -> Message -> IO (Maybe Response)
send cfg msg = do
  let opts = defaults & param "Authorization" .~ [_key cfg]
  retrying def (const $ return . isNothing) (\_ -> send' opts msg)

send' :: Options -> Message -> IO (Maybe Response)
send' opts msg = do
  r <- postWith opts gcmSendEndpoint (toJSON msg)
  let body = r ^. responseBody
  return $ decode body

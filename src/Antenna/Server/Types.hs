{-# LANGUAGE OverloadedStrings #-}

module Antenna.Server.Types where

import Data.Aeson
import qualified Database.Redis as Redis

data ServerConfig = ServerConfig
  { _port :: Int
  , _redisInfo :: Redis.ConnectInfo
  }

instance FromJSON ServerConfig where
  parseJSON =
    withObject "ServerConfig" $ \v -> ServerConfig
      <$> v .: "port"
      <*> (build <$> (v .: "redisHost") <*> (v .: "redisPort"))
    where
      build host port = Redis.defaultConnectInfo
        { Redis.connectHost = host
        , Redis.connectPort = Redis.PortNumber (fromInteger port)
        }



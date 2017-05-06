module Antenna.Server.Config (
  getServerConfig
  ) where

import Antenna.Types (Env(..))
import Antenna.Server.Types (ServerConfig(..))
import qualified Data.ByteString.Char8 as BS
import Data.Yaml (decodeFile)
import Data.List.Extra (splitOn)
import qualified Database.Redis as Redis
import System.Environment (getEnv)
import System.FilePath.Posix ((</>))

serverConfigBasePath :: FilePath
serverConfigBasePath = "config"

getServerConfigPath :: Env -> FilePath
getServerConfigPath Local = serverConfigBasePath </> "local.yaml"
getServerConfigPath Prod = serverConfigBasePath </> "prod.yaml"

parseRedisUrl :: String -> Redis.ConnectInfo
parseRedisUrl redisUrl =
  let [_, xs] = splitOn "//" redisUrl
      [_, ys, port] = splitOn ":" xs
      [passwd, host] = splitOn "@" ys
   in Redis.defaultConnectInfo
     { Redis.connectHost = host
     , Redis.connectPort = Redis.PortNumber (fromIntegral (read port))
     , Redis.connectAuth = Just (BS.pack passwd)
     }

getServerConfig :: Env -> IO ServerConfig
getServerConfig Local = do
  _config <- decodeFile (getServerConfigPath Local)
  case _config of
    Nothing -> error ("Cannot parse: " ++ getServerConfigPath Local)
    Just config -> pure config
getServerConfig Prod = do
  port <- read <$> getEnv "PORT"
  redisUrl <- getEnv "REDIS_URL"
  pure $ ServerConfig port (parseRedisUrl redisUrl)

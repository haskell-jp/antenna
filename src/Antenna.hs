{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Antenna (app) where

import Antenna.Cache (findCachedFeed, cacheFeed)
import Antenna.Config
import Antenna.Feed
import Antenna.Types
import Antenna.Server.Types
import Antenna.Server.Config
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (json)
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (sequence)
import qualified Database.Redis as Redis
import Network.Wai.Middleware.Cors (simpleCors)
import Web.Spock.Config
import Web.Spock

app :: Env -> IO ()
app env = do
  serverConfig <- getServerConfig env
  categories  <- getSourceList
  let sources = concatMap _sources categories
      createRedisConnection = Redis.connect (_redisInfo serverConfig)
      closeRedisConnection conn = () <$ Redis.runRedis conn Redis.quit
      redisConn = PCConn (ConnBuilder createRedisConnection closeRedisConnection (PoolCfg 1 1 30))
  spockCfg <- defaultSpockCfg () redisConn ()
  runSpock (_port serverConfig) $ spock spockCfg $ do
    middleware simpleCors
    post "menu" $ json categories
    post ("feed" <//> var) $ \labelsString -> do
      let labels = filter (not . Text.null) $ Text.splitOn "," labelsString
          findAtomItems label = sequence $ getAtomItems <$> find (\v -> _label v == label) sources
      asList <- catMaybes . map sequence . zip labels <$> mapM findAtomItems labels
      json $ createFeedJSON asList

getAtomItems :: Source -> SpockAction Redis.Connection sess st [AntennaItem]
getAtomItems source = do
  let label = _label source
  _cached <- findCachedFeed label
  case _cached of
    Nothing -> do
      feed <- liftIO $ getAntennaItems (_req source)
      cacheFeed label feed
      pure feed
    Just cached -> pure cached

createFeedJSON :: [(Text, [AntennaItem])] -> Value
createFeedJSON xs = object $ map (\(label, items) -> label .= items) xs


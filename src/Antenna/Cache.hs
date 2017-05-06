module Antenna.Cache where

import Antenna.Types
import Control.Monad.IO.Class (liftIO)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Database.Redis as Redis
import Web.Spock (SpockAction, runQuery)

findCachedFeed :: Text -> SpockAction Redis.Connection sess st (Maybe [AntennaItem])
findCachedFeed key = do
  result <- runQuery $ \conn ->
    Redis.runRedis conn $
      Redis.get (Text.encodeUtf8 key)
  case result of
    Left reply -> do
      liftIO $ putStrLn (show reply)
      pure Nothing
    Right _cached ->
      pure $ (Binary.decode . BS.fromStrict) <$> _cached

cacheFeed :: Text -> [AntennaItem] -> SpockAction Redis.Connection sess st ()
cacheFeed key items =
  let value = BS.toStrict $ Binary.encode items
   in runQuery $ \conn -> do
        Redis.runRedis conn $
          Redis.setex (Text.encodeUtf8 key) 300 value
        pure ()


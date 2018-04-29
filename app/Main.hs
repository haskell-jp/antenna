{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           RIO
import           RIO.FilePath       (dropFileName)
import qualified RIO.List           as L

import           Antenna.Config
import           Antenna.Html
import           Control.Monad      ((<=<))
import           Data.Extensible
import qualified Data.Yaml          as Y
import qualified ScrapBook
import           System.Environment (getArgs)

main :: IO ()
main = (listToMaybe <$> getArgs) >>= \case
  Nothing   -> error "please input config file path."
  Just path -> generate path =<< readConfig path

readConfig :: FilePath -> IO Config
readConfig = either (error . show) pure <=< Y.decodeFileEither

generate :: FilePath -> Config -> IO ()
generate path config = do
  posts <- fmap concat . forM sites $ \site ->
    ScrapBook.collect (ScrapBook.fetch site) `catch` handler

  writeFeed (dropFileName path ++ name)
      =<< ScrapBook.collect (ScrapBook.write sconfig feed' posts)

  writeHtml config "./index.html" $ tabNav (config ^. #baseUrl) Posts $ mapM_
    (postToHtml config)
    (take 50 . reverse $ L.sortOn (view #date) posts)

  writeHtml config "./sites.html" $ tabNav (config ^. #baseUrl) Sites $ mapM_
    (siteToHtml config)
    (L.sortOn (view #title) sites)
 where
   sconfig = toScrapBookConfig config
   name    = ScrapBook.fileName sconfig feed'
   sites   = fmap toSite $ config ^. #sites

handler :: MonadUnliftIO m => ScrapBook.CollectError -> m [ScrapBook.Post Site]
handler e = ScrapBook.collect (logError $ displayShow e) >> pure []

feed' :: ScrapBook.Format
feed' = embedAssoc $ #feed @= ()

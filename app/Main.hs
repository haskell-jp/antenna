{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Antenna.Html
import           Control.Lens       (view, (^.))
import           Control.Monad      ((<=<))
import           Data.Extensible
import           Data.List          (sortOn)
import           Data.Maybe         (listToMaybe)
import qualified Data.Yaml          as Y
import           ScrapBook          (collect, fetch, toSite, write)
import qualified ScrapBook
import           System.Environment (getArgs)
import           System.FilePath    (dropFileName)

main :: IO ()
main = (listToMaybe <$> getArgs) >>= \case
  Nothing   -> error "please input config file path."
  Just path -> generate path =<< readConfig path

readConfig :: FilePath -> IO ScrapBook.Config
readConfig = either (error . show) pure <=< decodeFileEither'
 where
  decodeFileEither' path =
    fmap (ScrapBook.updateFileName feed' path) <$> Y.decodeFileEither path

generate :: FilePath -> ScrapBook.Config -> IO ()
generate path config = either (error . show) pure <=< collect $ do
  let sites = fmap toSite $ config ^. #sites
  posts <- concat <$> mapM fetch sites

  writeFeed (dropFileName path ++ name) =<< write config feed' posts

  writeHtml config "./index.html" $ tabNav baseUrl Posts $ mapM_
    postToHtml
    (take 50 . reverse $ sortOn (view #date) posts)

  writeHtml config "./sites.html" $ tabNav baseUrl Sites $ mapM_
    siteToHtml
    (sortOn (view #title) sites)
 where
  name    = ScrapBook.fileName config feed'
  baseUrl = maybe "" (view #baseUrl) $ config ^. #feed

feed' :: ScrapBook.Format
feed' = embedAssoc $ #feed @= ()

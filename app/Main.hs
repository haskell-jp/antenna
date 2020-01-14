{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Paths_antenna          (version)
import           RIO
import           RIO.FilePath           (dropFileName)
import qualified RIO.List               as L

import           Antenna.Config
import           Antenna.Html
import           Control.Monad          ((<=<))
import           Data.Extensible
import           Data.Extensible.GetOpt
import qualified Data.Yaml              as Y
import           GetOpt                 (withGetOpt')
import           Mix
import           Mix.Plugin.Logger      as MixLogger
import qualified ScrapBook
import qualified Version

main :: IO ()
main = withGetOpt' "[options] [input-file]" opts $ \r args usage ->
  if | r ^. #help    -> hPutBuilder stdout (fromString usage)
     | r ^. #version -> hPutBuilder stdout (Version.build version)
     | otherwise     -> runCmd r $ listToMaybe args
  where
    opts = #help    @= helpOpt
        <: #version @= versionOpt
        <: #verbose @= verboseOpt
        <: nil

type Options = Record
  '[ "help"    >: Bool
   , "version" >: Bool
   , "verbose" >: Bool
   ]

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

type Env = Record
  '[ "logger" >: LogFunc
   , "config" >: Config
   ]

runCmd :: Options -> Maybe FilePath -> IO ()
runCmd _ Nothing        = error "please input config file path."
runCmd opts (Just path) = do
  config <- readConfig path
  let plugin = hsequence
             $ #logger <@=> MixLogger.buildPlugin logOpts
            <: #config <@=> pure config
            <: nil
  Mix.run plugin $ generate path
  where
    logOpts = #handle @= stdout
           <: #verbose @= (opts ^. #verbose)
           <: nil

readConfig :: FilePath -> IO Config
readConfig = either (error . show) pure <=< Y.decodeFileEither

generate :: FilePath -> RIO Env ()
generate path = do
  config <- asks (view #config)
  let sites = fmap toSite $ config ^. #sites
  MixLogger.logDebug $ "collect posts with: " <> fromString path
  posts <- fmap concat $
    runCollector (forM sites $ \site -> ScrapBook.fetch site `catch` handler)

  let sconfig  = toScrapBookConfig config
      feedName = dropFileName path <> ScrapBook.fileName sconfig feed'
  MixLogger.logDebug $ "write feed to: " <> fromString feedName
  writeFeed feedName =<< ScrapBook.collect (ScrapBook.write sconfig feed' posts)

  MixLogger.logDebug "write index.html"
  writeHtml config "./index.html" $ tabNav (config ^. #baseUrl) Posts $ mapM_
    (postToHtml config)
    (take 50 . reverse $ L.sortOn (view #date) posts)

  MixLogger.logDebug "write sites.html"
  writeHtml config "./sites.html" $ tabNav (config ^. #baseUrl) Sites $ mapM_
    (siteToHtml config)
    (L.sortOn (view #title) sites)

runCollector :: ScrapBook.Collecter a -> RIO Env a
runCollector act = do
  logger <- asks (view #logger)
  runRIO (ScrapBook.Env logger) act

handler ::
  (MonadIO m, MonadReader env m, HasLogFunc env)
  => ScrapBook.CollectError -> m [ScrapBook.Post Site]
handler e = MixLogger.logError (displayShow e) >> pure []

feed' :: ScrapBook.Format
feed' = embedAssoc $ #feed @= ()

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import qualified Git
import           Mix
import           Mix.Plugin.Logger      as MixLogger
import qualified Mix.Plugin.Shell       as MixShell
import           Options.Magic
import qualified ScrapBook
import           System.Cron            (addJob, execSchedule)
import qualified Version

main :: IO ()
main = withGetOpt' "[options] [input-file]" opts $ \r args ->
  let cmdEnchantments = shrink r :: Record CmdEnchantments
      defaultCmd      = Cmd $ \_ -> runCmd r (listToMaybe args)
  in exec (peel $ Continue defaultCmd `withEnchantment` cmdEnchantments)
  where
    opts = #help       @= helpOpt
        <: #version    @= versionOpt
        <: #daily      @= dailyOpt
        <: #hourly     @= hourlyOpt
        <: #minutely   @= minutelyOpt
        <: #verbose    @= verboseOpt
        <: #skip       @= skipOpt
        <: #withCopy   @= withCopyOpt
        <: #withCommit @= withCommitOpt
        <: #withPush   @= withPushOpt
        <: nil

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

dailyOpt :: OptDescr' Bool
dailyOpt = optFlag [] ["daily"] "Enable daily execute command"

hourlyOpt :: OptDescr' Bool
hourlyOpt = optFlag [] ["hourly"] "Enable houly execute command"

minutelyOpt :: OptDescr' Bool
minutelyOpt = optFlag [] ["minutely"] "Enable minutely execute command"

skipOpt :: OptDescr' Bool
skipOpt = optFlag [] ["skip"] "Skip generate HTML"

withCopyOpt :: OptDescr' Bool
withCopyOpt = optFlag [] ["with-copy"] "Copy files by another branch before generate HTML"

withCommitOpt :: OptDescr' Bool
withCommitOpt = optFlag [] ["with-commit"] "Create commit after generate HTML"

withPushOpt :: OptDescr' Bool
withPushOpt = optFlag [] ["with-push"] "Push commit after create commit"

type Options =
    Record (CmdEnchantments ++ LogOptEnchantemnts ++ ActionEnchantments)

type CmdEnchantments =
  '[ "help"       >: Bool
   , "version"    >: Bool
   , "daily"      >: Bool
   , "hourly"     >: Bool
   , "minutely"   >: Bool
   ]

newtype Cmd = Cmd { exec :: String -> IO () }

instance Magic (Enchantment (Breakable Cmd)) ("help" >: Bool) where
  magic _ = enchantmentIfTrue $ onlyOnce $ \_ ->
    Cmd $ \usage -> hPutBuilder stdout (fromString usage)

instance Magic (Enchantment (Breakable Cmd)) ("version" >: Bool) where
  magic _ = enchantmentIfTrue $ onlyOnce $ \_ ->
    Cmd $ \_ -> hPutBuilder stdout (Version.build version)

instance Magic (Enchantment (Breakable Cmd)) ("daily" >: Bool) where
  magic _ = enchantmentIfTrue $ onlyOnce $ \(Cmd cmd) ->
    Cmd $ \x -> cmd x `withCron` "0 8 * * *"

instance Magic (Enchantment (Breakable Cmd)) ("hourly" >: Bool) where
  magic _ = enchantmentIfTrue $ onlyOnce $ \(Cmd cmd) ->
    Cmd $ \x -> cmd x `withCron` "0 * * * *"

instance Magic (Enchantment (Breakable Cmd)) ("minutely" >: Bool) where
  magic _ = enchantmentIfTrue $ onlyOnce $ \(Cmd cmd) ->
    Cmd $ \x -> cmd x `withCron` "* * * * *"

type LogOptEnchantemnts =
  '[ "verbose" >: Bool
   ]

instance Magic (Enchantment MixLogger.MixLoggerConfig) ("verbose" >: Bool) where
  magic _ = Enchantment . set #verbose

type ActionEnchantments =
  '[ "skip"       >: Bool
   , "withCopy"   >: Bool
   , "withCommit" >: Bool
   , "withPush"   >: Bool
   ]

instance Magic (Enchantment (RIO Env ())) ("skip" >: Bool) where
  magic _ = enchantmentIfTrue $ \_ -> pure ()

instance Magic (Enchantment (RIO Env ())) ("withCopy" >: Bool) where
  magic _ = enchantmentIfTrue $ \act -> do
    copyFilesByAnotherBranch
    act

instance Magic (Enchantment (RIO Env ())) ("withCommit" >: Bool) where
  magic _ = enchantmentIfTrue $ \act -> do
    MixShell.exec (Git.pull [])
    act
    commitGeneratedFiles

instance Magic (Enchantment (RIO Env ())) ("withPush" >: Bool) where
  magic _ = enchantmentIfTrue $ \act -> do
    act
    pushCommit

type Env = Record
  '[ "logger" >: LogFunc
   , "config" >: Config
   , "work"   >: FilePath
   ]

runCmd :: Options -> Maybe FilePath -> IO ()
runCmd _ Nothing        = error "please input config file path."
runCmd opts (Just path) = do
  config <- readConfig path
  let plugin = hsequence
             $ #logger <@=> MixLogger.buildPlugin logOpts'
            <: #config <@=> pure config
            <: #work   <@=> pure "."
            <: nil
  Mix.run plugin $
    generate path `withEnchantment` (shrink opts :: Record ActionEnchantments)
  where
    logOpts = #handle  @= stdout
           <: #verbose @= False
           <: nil
    logOpts' = logOpts `withEnchantment` (shrink opts :: Record LogOptEnchantemnts)


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

copyFilesByAnotherBranch :: RIO Env ()
copyFilesByAnotherBranch = do
  targets <- view #copy <$> asks (gitConfig . view #config)
  MixLogger.logDebug $ "copy files by " <> displayShow targets
  MixShell.exec $ forM_ (splitCopyTarget <$> targets) $ \(branch, path) ->
    Git.checkout [branch, "--", path]

commitGeneratedFiles :: RIO Env ()
commitGeneratedFiles = do
  files <- view #files <$> asks (gitConfig . view #config)
  MixLogger.logDebug $ "create commit with " <> displayShow files
  MixShell.exec $ do
    Git.add files
    changes <- Git.diffFileNames ["--staged"]
    when (not $ null changes) $ Git.commit ["-m", message]
  where
    message = "[skip ci] Update planet haskell. See https://haskell.jp/antenna/ for new entries!"

pushCommit :: RIO Env ()
pushCommit = do
  branch <- view #branch <$> asks (gitConfig . view #config)
  MixLogger.logDebug $ "push commit to origin/" <> display branch
  MixShell.exec (Git.push ["origin", branch])

withCron :: IO () -> Text -> IO ()
withCron act t = do
  _ <- execSchedule $ addJob act t
  forever $ threadDelay maxBound

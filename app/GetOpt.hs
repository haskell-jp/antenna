{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds  #-}

module GetOpt
    ( withGetOpt'
    ) where

import           RIO

import           Data.Extensible
import           Data.Extensible.GetOpt (OptionDescr, getOptRecord)
import           System.Environment     (getArgs, getProgName)
import           System.Exit            (die)
import           System.IO              (hPutStrLn)

withGetOpt' :: MonadIO m => String -- ^ Non-option usage
  -> RecordOf (OptionDescr h) xs -- ^ option desciptors
  -> (RecordOf h xs -> [String] -> String -> m a) -- ^ the result, non-option arguments and usage
  -> m a
withGetOpt' nonOptUsage descs k =
  getOptRecord descs <$> liftIO getArgs >>= \case
    (r, xs, [],  usage) -> liftIO (mkUsage usage) >>= k r xs
    (_, _, errs, usage) -> liftIO $ do
      mapM_ (hPutStrLn stderr) errs
      mkUsage usage >>= die
  where
    mkUsage usage = usage . (++ (' ' : nonOptUsage)) <$> getProgName

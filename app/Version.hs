{-# LANGUAGE TemplateHaskell  #-}

module Version
  ( build
  ) where

import           RIO

import           Data.Version (Version)
import qualified Data.Version as Version
import qualified GitHash

build :: Version -> Builder
build v = toBuilder $ unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , GitHash.giCommitDate gi
  , "(" ++ show (GitHash.giCommitCount gi) ++ " commits)"
  ]
  where
    gi = $$(GitHash.tGitInfoCwd)

toBuilder :: String -> Builder
toBuilder = encodeUtf8Builder . fromString

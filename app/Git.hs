{-# LANGUAGE OverloadedStrings #-}

module Git where

import           RIO
import qualified RIO.Text as Text

import           Shelly   hiding (FilePath, unlessM)

pull :: [Text] -> Sh ()
pull = command1_ "git" [] "pull"

push :: [Text] -> Sh ()
push = command1_ "git" [] "push"

commit :: [Text] -> Sh ()
commit = command1_ "git" [] "commit"

add :: [Text] -> Sh ()
add = command1_ "git" [] "add"

diffFileNames :: [Text] -> Sh [Text]
diffFileNames opts = Text.lines <$> command1 "git" [] "diff" ("--name-only" : opts)


module Antenna.Config where

import Antenna.Types (Env(..), Category)

import Data.Semigroup ((<>))
import Data.Yaml (decodeFile)
import Options.Applicative

envFromString :: String -> Maybe Env
envFromString "local" = Just Local
envFromString "prod" = Just Prod
envFromString _ = Nothing

envParser :: Parser Env
envParser = option (maybeReader envFromString)
  (  long "env"
  <> short 'e'
  <> help "environment"
  )

envParserInfo :: ParserInfo Env
envParserInfo = info envParser fullDesc

getEnv :: IO Env
getEnv = execParser envParserInfo

getSourceList :: IO [Category]
getSourceList = do
  _categories <- decodeFile "config/feeds.yaml"
  case _categories of
    Nothing -> pure []
    Just categories -> pure categories


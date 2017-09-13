{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Antenna.Types where
import Data.Aeson
import Data.Binary
import Data.Text (Text)
import Data.Time.LocalTime (ZonedTime)
import Data.Time.Format
import GHC.Generics (Generic)
import Network.HTTP.Simple (Request, parseRequest_, setRequestHeader)
import Text.Atom.Types (AtomFeed)
import Text.RSS.Types (RssDocument)

data Env = Local | Prod

data AntennaItem = AntennaItem
  { _title :: Text
  , _urls :: [Text]
  , _datetime :: ZonedTime
  } deriving (Generic)

instance Binary ZonedTime where
  put = put . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%z"))
  get = parseTimeOrError False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%z")) <$> get

instance Binary AntennaItem

instance ToJSON AntennaItem where
  toJSON (AntennaItem title urls datetime) =
    let formatted = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) datetime
     in object ["title" .= title, "urls" .= urls, "datetime" .= formatted]

data Category = Category
  { _cname :: String
  , _showDefault :: Bool
  , _sources :: [Source]
  }

instance FromJSON Category where
  parseJSON =
    withObject "Category" $ \v -> Category
      <$> v .: "name"
      <*> v .: "show-default"
      <*> v .: "sources"

instance ToJSON Category where
  toJSON (Category name showDefault sources) =
    object ["name" .= name, "show-default" .= showDefault, "sources" .= sources]

data Source = Source
  { _name :: String
  , _label :: Text
  , _url :: String
  , _req :: Request
  }

instance FromJSON Source where
  parseJSON =
    withObject "Source" $ \v -> Source
      <$> v .: "name"
      <*> v .: "label"
      <*> v .: "url"
      <*> (setRequestHeader "User-Agent" ["Haskell Antenna"] . parseRequest_ <$> v .: "url")

instance ToJSON Source where
  toJSON (Source name label url _) =
    object ["name" .= name, "label" .= label, "url" .= url]

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Antenna.Config where

import           Control.Applicative (Alternative (..))
import           Control.Lens        (view, (&), (.~), (^.))
import           Data.Default        (def)
import           Data.Extensible
import           Data.List           (find)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified ScrapBook

type Config = Record
  '[ "title"       >: Text
   , "baseUrl"     >: Text
   , "feedName"    >: Text
   , "blankAvatar" >: Text
   , "sites"       >: [SiteConfig]
   ]

toScrapBookConfig :: Config -> ScrapBook.Config
toScrapBookConfig config =
  def & #feed .~ Just feedConfig & #sites .~ (shrink <$> config ^. #sites)
  where
    feedConfig = shrink $ #name @= Just (config ^. #feedName) <: config

type SiteConfig = Record
  '[ "title"  >: Text
   , "author" >: Text
   , "url"    >: Text
   , "logo"   >: Maybe ImageConfig  -- ^ add to ScrapBook config
   , "feed"   >: Maybe Text
   , "atom"   >: Maybe ScrapBook.AtomConfig
   , "rss"    >: Maybe Text
   ]

type ImageConfig = Record
  '[ "url"    >: Maybe Text
   , "github" >: Maybe Text
   ]

imagePath :: ImageConfig -> Maybe Text
imagePath config
    = mappend "https://avatars.githubusercontent.com/" <$> (config ^. #github)
  <|> (config ^. #url)

imagePath' :: Config -> ScrapBook.Site -> Text
imagePath' config site =
  fromMaybe (config ^. #blankAvatar) $ imagePath =<< view #logo
    =<< find ((==) site . ScrapBook.toSite . shrink) (config ^. #sites)

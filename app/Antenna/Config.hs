{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Antenna.Config where

import           RIO

import           Data.Default    (def)
import           Data.Extensible
import qualified ScrapBook

type Config = Record
  '[ "title"       >: Text
   , "baseUrl"     >: Text
   , "feedName"    >: Text
   , "blankAvatar" >: Text
   , "logo"        >: Text
   , "favicon"     >: Text
   , "sites"       >: [SiteConfig]
   ]

toSite :: SiteConfig -> Site
toSite conf =
  shrinkAssoc $ #logo @= (conf ^. #logo) <: ScrapBook.toSite (shrinkAssoc conf)

toScrapBookConfig :: Config -> ScrapBook.Config
toScrapBookConfig config = def & #feed `set` Just feedConfig
  where
    feedConfig = shrink $ #name @= Just (config ^. #feedName) <: config

type Site = Record
  (ScrapBook.SiteFields ++ '[ "logo" >: Maybe ImageConfig ])

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
   , "hatena" >: Maybe Text
   ]

imagePath :: ImageConfig -> Maybe Text
imagePath config
    = mappend "https://avatars.githubusercontent.com/" <$> (config ^. #github)
  <|> hatenaProfileImageLink <$> (config ^. #hatena)
  <|> (config ^. #url)
  where
    hatenaProfileImageLink user =
      mconcat ["https://cdn.profile-image.st-hatena.com/users/", user, "/profile.png"]

imagePath' :: Config -> Site -> Text
imagePath' config site =
  fromMaybe (config ^. #blankAvatar) $ imagePath =<< (site ^. #logo)

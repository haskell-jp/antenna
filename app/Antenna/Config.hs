{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Antenna.Config where

import           Control.Lens    ((&), (.~), (^.))
import           Data.Default    (def)
import           Data.Extensible
import           Data.Text       (Text)
import qualified ScrapBook

type Config = Record
  '[ "title"    >: Text
   , "baseUrl"  >: Text
   , "feedName" >: Text
   , "sites"    >: [SiteConfig]
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
   , "feed"   >: Maybe Text
   , "atom"   >: Maybe ScrapBook.AtomConfig
   , "rss"    >: Maybe Text
   ]

{-# LANGUAGE OverloadedStrings #-}

module Antenna.Feed where

import Antenna.Types
import Data.ByteString.Lazy (ByteString)
import Data.Conduit
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeZone(..), ZonedTime, utcToZonedTime)
import Network.HTTP.Simple (httpLBS, getResponseBody, Request, parseRequest_)
import Text.Atom.Types
import Text.Atom.Conduit.Parse
import Text.RSS.Types
import Text.RSS.Conduit.Parse
import Text.RSS1.Conduit.Parse
import Text.XML.Stream.Parse
import URI.ByteString

data Feed = Rss RssDocument | Atom AtomFeed

getAntennaItems :: Request -> IO [AntennaItem]
getAntennaItems req = do
  body <- getResponseBody <$> httpLBS req
  convert <$> parseFeed body

parseFeed :: ByteString -> IO Feed
parseFeed xs =
  runConduit $
    parseLBS def xs =$= force "Invalid feed" (parseAtom `orE` parseRss `orE` parseRss1)
      where
        parseAtom = fmap Atom <$> atomFeed
        parseRss = fmap Rss <$> rssDocument
        parseRss1 = fmap Rss <$> rss1Document

jstTimeZone :: TimeZone
jstTimeZone = TimeZone 540 False "JST" -- +09:00 = 9h * 60m = 540m

toZonedTime :: UTCTime -> ZonedTime
toZonedTime utcTime = utcToZonedTime jstTimeZone utcTime

convert :: Feed -> [AntennaItem]
convert (Rss feed) = map f $ channelItems feed
  where
    f :: RssItem -> AntennaItem
    f item = AntennaItem
      { _title = (itemTitle item)
      , _urls = (g $ itemLink item)
      , _datetime = (maybe undefined toZonedTime $ itemPubDate item)
      }
    g :: Maybe RssURI -> [Text]
    g Nothing = []
    g (Just (RssURI uri)) = [Text.decodeUtf8 $ serializeURIRef' uri]
convert (Atom feed) = map f $ feedEntries feed
  where
    f :: AtomEntry -> AntennaItem
    f entry = AntennaItem
      { _title = (g $ entryTitle entry)
      , _urls = (map (h . linkHref) $ entryLinks entry)
      , _datetime = (toZonedTime $ entryUpdated entry)
      }
    g :: AtomText -> Text
    g (AtomPlainText _ text) = text
    g (AtomXHTMLText text) = text
    h :: AtomURI -> Text
    h (AtomURI uri) = Text.decodeUtf8 $ serializeURIRef' uri


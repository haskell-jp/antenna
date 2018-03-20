{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Antenna.Html where

import           Prelude                       hiding (div, head, id, span)

import           Antenna.Config
import           Control.Lens                  ((^.))
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.String                   (IsString, fromString)
import           Data.Text                     (Text, unpack)
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy.IO             as TL
import           Data.Time
import qualified ScrapBook
import           System.Directory              (createDirectoryIfMissing)
import           System.FilePath               (dropFileName)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes   (class_, height, href, id, lang,
                                                rel, src, type_, width)

data Tab
  = Posts
  | Sites
  deriving (Show, Eq)

tabNav :: Text -> Tab -> Html -> Html
tabNav baseUrl selectedTab list = do
  div ! class_ "tabnav" $ do
    div ! class_ "float-right" $
      a ! href (addBaseUrl "/feed") ! class_ "tabnav-extra" $ "feed"
    nav ! class_ "tabnav-tabs" $ do
      tab "/"      (selectedTab == Posts) "Post"
      tab "/sites" (selectedTab == Sites) "Sites"
  ul ! class_ "list-style-none" $ list
 where
  tab path selected = a ! href (addBaseUrl path) !
    class_ ("tabnav-tab " `mappend` if selected then "selected" else "")
  addBaseUrl = fromText . mappend baseUrl

writeFeed :: MonadIO m => FilePath -> Text -> m ()
writeFeed path txt = liftIO $ writeFileWithDir path txt

writeHtml :: MonadIO m => Config -> FilePath -> Html -> m ()
writeHtml config path bodyHtml =
  liftIO . TL.writeFile path . renderHtml $ docTypeHtml ! lang "jp" $ do
    head $ do
      title $ toHtml (config ^. #title)
      link ! rel "stylesheet" ! type_ "text/css" ! href
        "https://cdnjs.cloudflare.com/ajax/libs/Primer/10.0.0-rc.21/build.css"
    body $ div ! class_ "container-md" $ do
      h1 ! id "header" $
        toHtml (config ^. #title)
      bodyHtml

postToHtml :: Config -> ScrapBook.Post -> Html
postToHtml config post = li ! class_ "d-flex border-bottom py-2" $ do
  span ! class_ "m-2 mr-3" $
    img ! class_ "avatar avatar-small" ! width "32" ! height "32"
        ! src (fromText $ imagePath' config $ post ^. #site)
  div ! class_ "d-flex flex-justify-between flex-items-baseline width-full" $
    div $ do
      h3 $ a' ! href (fromText $ post ^. #url) $ toHtml (post ^. #title)
      div $ do
        let site = post ^. #site
        toHtml $ mconcat ["by ", site ^. #author]
        " on "
        span $ a' ! href (fromText $ site ^. #url) $ toHtml (site ^. #title)
        toHtml $ mconcat [" at ", formatTimeToDate $ unpack (post ^. #date)]

siteToHtml :: Config -> ScrapBook.Site -> Html
siteToHtml config site = li ! class_ "d-flex border-bottom py-2" $ do
  span ! class_ "m-2 mr-3" $
    img ! class_ "avatar avatar-small" ! width "32" ! height "32"
        ! src (fromText $ imagePath' config site)
  div ! class_ "d-flex flex-justify-between flex-items-baseline width-full" $
    div $ do
      h3 $ a' ! href (fromText $ site ^. #url) $ toHtml (site ^. #title)
      div $ toHtml $ mconcat ["by ", site ^. #author]

a' :: Html -> Html
a' = a ! class_ "link-gray-dark"

writeFileWithDir :: FilePath -> Text -> IO ()
writeFileWithDir path txt = do
  createDirectoryIfMissing True $ dropFileName path
  T.writeFile path txt

fromText :: IsString a => Text -> a
fromText = fromString . unpack

formatTimeToDate :: String -> String
formatTimeToDate =
  maybe "" (formatTime defaultTimeLocale "%B %d, %Y") . formatTimeFromRFC3339

-- take 10 == take (length "2018-02-02")
formatTimeFromRFC3339 :: String -> Maybe UTCTime
formatTimeFromRFC3339 =
  parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) . take 10

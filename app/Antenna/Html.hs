{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Antenna.Html where

import           RIO                           hiding (div, link, span)
import           RIO.Directory                 (createDirectoryIfMissing)
import           RIO.FilePath                  (dropFileName)
import qualified RIO.Text                      as Text
import qualified RIO.Text.Lazy                 as TL
import           RIO.Time

import           Antenna.Config
import qualified ScrapBook
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes   (class_, height, href, lang, rel,
                                                src, type_, width)
import qualified Text.Blaze.Html5.Attributes   as Attr

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
  liftIO . writeFileUtf8 path . TL.toStrict . renderHtml $
    docTypeHtml ! lang "jp" $ do
      head $ do
        title $ toHtml (config ^. #title)
        link ! rel "stylesheet" ! type_ "text/css"
             ! href "https://cdnjs.cloudflare.com/ajax/libs/Primer/10.0.0-rc.21/build.css"
        link ! rel "icon" ! type_ "image/png"
             ! href (fromText $ config ^. #favicon)
      body $ div ! class_ "container-md" $ do
        h1 ! Attr.id "header" $ do
          span $ toHtml (config ^. #title)
          img ! class_ "float-right mt-2" ! height "32"
              ! src (fromText $ config ^. #logo)
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
        toHtml $ mconcat [" at ", formatTimeToDate $ Text.unpack (post ^. #date)]

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
  writeFileUtf8 path txt

fromText :: IsString a => Text -> a
fromText = fromString . Text.unpack

formatTimeToDate :: String -> String
formatTimeToDate =
  maybe "" (formatTime defaultTimeLocale "%B %d, %Y") . formatTimeFromRFC3339

-- take 10 == take (length "2018-02-02")
formatTimeFromRFC3339 :: String -> Maybe UTCTime
formatTimeFromRFC3339 =
  parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) . take 10

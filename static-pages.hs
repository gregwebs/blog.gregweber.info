{-# LANGUAGE TypeFamilies, TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Yesod
import Text.Shakespeare.Text
import Text.Markdown
import Yesod.Static
import Network.WAI.Application.StaticPages (parseRoutePaths, renderStaticPages)
-- import Text.Hamlet
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Control.Monad (forM_, liftM)

import Prelude hiding (FilePath)
import Settings.StaticFiles
import Data.Monoid ((<>), mempty)

import Text.Hamlet (hamletFile)

import Shelly
import Filesystem.Path.CurrentOS (encodeString, dropExtension, replaceExtensions)
import qualified Data.Text.Lazy as LT
import qualified Data.HashMap.Strict as M

import Data.Yaml (decode)
import Data.Text.Encoding (encodeUtf8)
import Data.List (sort)

default (LT.Text)

-- what package should this go in?
renderMarkdownFile :: FilePath -> IO ((M.HashMap Text Text), Html)
renderMarkdownFile file = do
    contents <- shelly (readfile filename)
    let (frontMatter, rest) = case LT.stripPrefix "---" contents of 
            Nothing -> (mempty, contents)
            Just firstMarkerStripped ->
                LT.breakOn "---\n" (LT.stripStart firstMarkerStripped)
    let mResult = decode $ encodeUtf8 $ LT.toStrict $ frontMatter
    return (maybe M.empty id mResult, markdown def { msXssProtect = False } rest)
  where
    filename = "posts" </> file <.> "markdown"


data StaticPages = StaticPages {getStatic :: Static}

mkYesod "StaticPages" [parseRoutes|
/static           StaticR Static getStatic
/                 HomeR   GET
/posts            PostsR  GET
/posts/#Text      PostR   GET
/tags/#Text       TagR   GET
/rss/all.xml      RssR    GET
/atom/all.xml     AtomR   GET
|]


-- | TODO: perhaps can inspect the routes
staticPageRoutePaths :: [T.Text]
staticPageRoutePaths = parseRoutePaths [st|
-- /
/posts
-- /rss/all.xml
-- /atom/all.xml
|]

blogTitle, blogAuthor :: Text
blogTitle = "Greg Weber's Programming Blog"
blogAuthor = "Greg Weber"

instance Yesod StaticPages where
    jsLoader _ = BottomOfBody
    defaultLayout widget = do
        pc <- widgetToPageContent $ do
            widget
            $(combineStylesheets 'StaticR [
                css_screen_css
              , css_syntax_css
              ])
        let sidebar = [hamlet|<div>|]
        hamletToRepHtml $(hamletFile "templates/default.html.hamlet")


getHomeR :: Handler Html
getHomeR = do
  posts <- liftIO getPosts
  let body = "" :: Html
  let tagcloud = "" :: Html
  defaultLayout $ do
    setTitle "Home"
    $(whamletFile "templates/index.html.hamlet")

getPostR :: Text -> Handler Html
getPostR post = do
  let urlFp = fromText post
  let url = toTextIgnore urlFp
  (frontMatter, content) <- liftIO $ renderMarkdownFile urlFp
  let date = LT.intercalate "-" $ take 3 $ LT.splitOn "-" url
  let dateTime = date
  let title = lookupMempty "title" frontMatter
  let tags = lookupMempty "tags" frontMatter
  defaultLayout $ do
    setTitle $ toHtml title
    $(whamletFile "templates/post.html.hamlet")
  where
    lookupMempty = M.lookupDefault mempty

getPosts = liftM (reverse . sort) $ shelly $ find "posts"

getTagR :: Text -> Handler Html
getTagR tag = undefined

getPostsR :: Handler Html
getPostsR = do
  let title = "All Posts"
  posts <- liftIO getPosts
  let body = "" :: Text
  -- let postItem = $(hamletFile "templates/post-item.html.hamlet")
  defaultLayout $ do
    $(whamletFile "templates/posts.html.hamlet")

getRssR :: Handler RepXml
getRssR = undefined

getAtomR :: Handler RepXml
getAtomR = undefined

main :: IO ()
main = do
  app <- toWaiAppPlain $ StaticPages undefined
  let renderPages = renderStaticPages app "site/"
  renderPages staticPageRoutePaths 
  postPaths <- map dropExtension `fmap` getPosts
  renderPages (map (LT.toStrict . toTextIgnore) postPaths)
  -- tagsPaths <- undefined

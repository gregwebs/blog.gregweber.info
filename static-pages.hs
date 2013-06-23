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
import Data.Text (Text)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Control.Monad (forM_, liftM)

import Prelude hiding (FilePath)
import Settings.StaticFiles
import Data.Monoid ((<>), mempty)

import Text.Hamlet (hamletFile)

import Shelly hiding (trace)
import Filesystem.Path.CurrentOS (encodeString, dropExtension, replaceExtensions, filename)
import qualified Data.Text.Lazy as LT
import qualified Data.HashMap.Strict as M

import Data.Yaml (decode)
import Data.Text.Encoding (encodeUtf8)
import Data.List (sort)
import Data.Maybe (fromJust)
import Debug.Trace
import Control.Monad (unless)
import Safe
import Data.Char (toUpper, toLower)

default (Text)

traceIt :: Show s => s -> s
traceIt a = trace (show a) a

renderMarkdownFileJust :: FilePath -> IO (((M.HashMap Text Text)), Html) -- HashMap is YAML frontmatter
renderMarkdownFileJust filename = do
  (frontMatter, html) <- renderMarkdownFile filename
  unless (not (M.null frontMatter)) $ error "no front matter found"
  return (frontMatter, html)

-- what package should this go in?
renderMarkdownFile :: FilePath
                   -> IO ((M.HashMap Text Text), Html) -- HashMap is YAML frontmatter
renderMarkdownFile filename = do
    contents <- shelly (readfile filename)
    let (frontMatter, rest) = case T.stripPrefix "---" contents of 
            Nothing -> (mempty, contents)
            Just firstMarkerStripped ->
                T.breakOn "---\n" (T.stripStart firstMarkerStripped)
    let mResult = decode $ encodeUtf8 $ frontMatter
    return (maybe M.empty id mResult, markdown def { msXssProtect = False } $ LT.fromStrict rest)

capitalize :: String -> String
capitalize (c:str) = toUpper c : str


data Tag = Ruby | Haskell | Deploy | Fibers | Mongodb | Yesod | Linux
           deriving (Read, Show, Eq)

instance PathPiece Tag where
  toPathPiece = T.pack . map toLower . show
  fromPathPiece = Just . readNote "tag piece" . capitalize . T.unpack

data Post = Post
     { postUrl :: Text
     , postTitle :: Text
     , postContent :: Html
     , postDate :: Text
     , postTags :: [Tag]
     }


data StaticPages = StaticPages {getStatic :: Static}

mkYesod "StaticPages" [parseRoutes|
/static           StaticR Static getStatic
/                 HomeR   GET
/posts            PostsR  GET
/posts/#Text      PostR   GET
/tags/#Tag        TagR   GET
/rss/all.xml      RssR    GET
/atom/all.xml     AtomR   GET
|]


-- | TODO: perhaps can inspect the routes
staticPageRoutePaths :: [T.Text]
staticPageRoutePaths = parseRoutePaths [st|
/
/posts
/tags/ruby
/tags/haskell
/tags/deploy
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


-- renderPostItem :: Post -> Widget
renderPostItem post = $(whamletFile "templates/post-preview.html.hamlet")
  where
    tags post = do
      [whamlet|
        $forall tag <- postTags post
          <a href=@{TagR tag}>#{show tag}
      |]

getHomeR :: Handler Html
getHomeR = do
  posts <- loadPosts
  let tagcloud = "" :: Html
  let body = "" :: Html
  defaultLayout $ do
    setTitle "Home"
    $(whamletFile "templates/index.html.hamlet")

dateTimeFromPostPath :: FilePath -> Text
dateTimeFromPostPath url = 
  T.intercalate "-" $ take 3 $ T.splitOn "-" $ toTextIgnore url

getPostR :: Text -> Handler Html
getPostR post = do
  let urlFp = fromText post
  let url = toTextIgnore urlFp
  (frontMatter, content) <- liftIO $ renderMarkdownFileJust $ "posts" </> urlFp <.> "markdown"
  let date = dateTimeFromPostPath urlFp
  let dateTime = date
  let title = lookupMempty "title" frontMatter
  let tags = lookupMempty "tags" frontMatter
  defaultLayout $ do
    setTitle $ toHtml title
    $(whamletFile "templates/post.html.hamlet")
  where
    lookupMempty = M.lookupDefault mempty

getPostFiles = liftM (reverse . sort) $ shelly $ find "posts"

loadPosts = do
    postFiles <- liftIO getPostFiles 
    postMatters <- liftIO (mapM renderMarkdownFileJust postFiles)
    return $ map mkPost (zip postFiles postMatters)
  where
    formattedDate p = postDate p
    mkPost (fp, (frontMatter, html)) =
      Post { postUrl = toTextIgnore  $ dropExtension fp
           , postTitle = maybe "" id $ M.lookup "title" frontMatter 
           , postContent = html
           , postDate = dateTimeFromPostPath $ filename fp
           , postTags = maybe [] readTags $ M.lookup "tags" frontMatter 
           }
    readTags = map (readNote "tagsplit") . map (capitalize) . map T.unpack . T.split (== ',') 

getTagR :: Tag -> Handler Html
getTagR tag = do
    allPosts <- loadPosts
    let posts = filter (\p -> tag `elem` postTags p) allPosts
    let title = "Posts tagged " <> show tag
    defaultLayout $ do
      $(whamletFile "templates/posts.html.hamlet")

getPostsR :: Handler Html
getPostsR = do
    posts <- loadPosts
    let title = "All Posts"
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
  postPaths <- map dropExtension `fmap` getPostFiles
  renderPages $ map toTextIgnore postPaths
  -- tagsPaths <- undefined

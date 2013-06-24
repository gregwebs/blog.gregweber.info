{-# LANGUAGE TypeFamilies, TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
-- | Major Generation Assumption: blog psots are stored in /posts
-- With the format YYYY-MM-DD-title-name.markdown
--
-- Major Deployment assumption: Web server will serve up foo.html when it receives a request for foo
-- index.html will be served at the root
module Main where
--
import Yesod
import Text.Shakespeare.Text
import Text.Markdown
import Yesod.Static
import Network.WAI.Application.StaticPages (parseRoutePaths, renderStaticPages)
-- import Text.Hamlet
import qualified Data.Text as T
import Data.Text (Text)

import Prelude hiding (FilePath)
import Settings.StaticFiles
import Data.Monoid ((<>), mempty)

import Text.Hamlet (hamletFile)

import Shelly hiding (trace, tag, unless)
import Filesystem.Path.CurrentOS (dropExtension, filename)
import qualified Data.Text.Lazy as LT
import qualified Data.HashMap.Strict as M

import Data.Yaml (decode)
import Data.Text.Encoding (encodeUtf8)
import Data.List (sort)
import Control.Monad (liftM, unless)
import Safe
import Data.Char (toUpper, toLower)
import Data.Time.Clock (getCurrentTime)

import Yesod.AtomFeed
import Yesod.RssFeed

{-
import Debug.Trace
traceIt :: Show s => s -> s
traceIt a = trace (show a) a
-}

default (Text)

renderMarkdownFileJust :: FilePath -> IO (((M.HashMap Text Text)), Html) -- HashMap is YAML frontmatter
renderMarkdownFileJust fp = do
  (frontMatter, html) <- renderMarkdownFile fp
  unless (not (M.null frontMatter)) $ error "no front matter found"
  return (frontMatter, html)

-- what package should this go in?
renderMarkdownFile :: FilePath
                   -> IO ((M.HashMap Text Text), Html) -- HashMap is YAML frontmatter
renderMarkdownFile fp = do
    contents <- shelly $ readfile fp
    let (frontMatter, rest) = case T.stripPrefix "---" contents of 
            Nothing -> (mempty, contents)
            Just firstMarkerStripped ->
                T.breakOn "---\n" (T.stripStart firstMarkerStripped)
    let mResult = decode $ encodeUtf8 $ frontMatter
    return (maybe M.empty id mResult, markdown def { msXssProtect = False } $ LT.fromStrict rest)

capitalizeST :: String -> Text
capitalizeST = T.pack . capitalize
capitalizeTS :: Text -> String
capitalizeTS = capitalize . T.unpack

capitalize :: String -> String
capitalize (c:str) = toUpper c : str
capitalize [] = []


data Tag = Ruby | Haskell | Deploy | Fibers | Mongodb | Yesod | Linux | All
           deriving (Read, Show, Eq)

instance PathPiece Tag where
  toPathPiece = T.pack . map toLower . show
  fromPathPiece = Just . readNote "tag piece" . capitalizeTS

data Post = Post
     { postUrl :: Text
     , postTitle :: Text
     , postContent :: Html
     , postDate :: Text
     , postTags :: [Tag]
     }


data StaticPages = StaticPages {getStatic :: Static}

mkYesod "StaticPages" [parseRoutes|
/static           StaticR  Static getStatic
/                 HomeR    GET
/posts            PostsR   GET
/posts/#Text      PostR    GET
/tags/#Tag        TagR     GET
/rss/#Tag         RssTagR  GET
/atom/#Tag        AtomTagR GET
|]


-- | TODO: perhaps can inspect the routes
staticPageRoutePaths :: [T.Text]
staticPageRoutePaths = parseRoutePaths [st|
/
/posts
/tags/ruby
/tags/haskell
/tags/deploy
/rss/all
/atom/all
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
        posts <- take 3 `fmap` liftIO loadPosts
        let recentPosts = $(hamletFile "templates/recent-posts.html.hamlet")
        let menu = $(hamletFile "templates/menu.html.hamlet")
        let sidebar = $(hamletFile "templates/sidebar.html.hamlet")
        hamletToRepHtml $(hamletFile "templates/default.html.hamlet")


renderPostItem :: Post -> Widget
renderPostItem post = $(whamletFile "templates/post-preview.html.hamlet")
  where
    tags p = [whamlet|
        $forall tag <- postTags p
          <a href=@{TagR tag}>#{show tag}
      |]

getHomeR :: Handler Html
getHomeR = do
    posts <- liftIO loadPosts
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

getPostFiles :: IO [FilePath]
getPostFiles = liftM (reverse . sort) $ shelly $ find "posts"

loadPosts :: IO [Post]
loadPosts = do
    postFiles <- liftIO getPostFiles 
    postMatters <- liftIO (mapM renderMarkdownFileJust postFiles)
    return $ map mkPost (zip postFiles postMatters)
  where
    -- formattedDate p = postDate p
    mkPost (fp, (frontMatter, html)) =
      Post { postUrl = toTextIgnore  $ dropExtension fp
           , postTitle = maybe "" id $ M.lookup "title" frontMatter 
           , postContent = html
           , postDate = dateTimeFromPostPath $ filename fp
           , postTags = maybe [] readTags $ M.lookup "tags" frontMatter 
           }
    readTags = map (readNote "tagsplit") . map (capitalizeTS) . T.split (== ',') 

loadTagPosts :: Tag -> IO [Post]
loadTagPosts tag = loadPosts >>= return .  filter (\p -> tag `elem` postTags p)

getTagR :: Tag -> Handler Html
getTagR tag = do
    posts <- liftIO $ loadTagPosts tag
    let title = "Posts tagged " <> show tag
    defaultLayout $ do
      $(whamletFile "templates/posts.html.hamlet")

getPostsR :: Handler Html
getPostsR = do
    posts <- liftIO loadPosts
    let title = "All Posts"
    -- let postItem = $(hamletFile "templates/post-item.html.hamlet")
    defaultLayout $ do
      $(whamletFile "templates/posts.html.hamlet")

taggedFeed :: Route StaticPages -> Tag -> Handler (Feed (Route StaticPages))
taggedFeed selfUrl tag = do
  posts <- liftIO $ case tag of
                     All -> loadPosts
                     _ -> loadTagPosts tag
  now <- liftIO getCurrentTime
  return $ Feed
        { feedTitle       = (capitalizeST $ show tag) <> " Posts"
        , feedDescription = toHtml $ (capitalizeST $ show tag) <> " posts from " <> blogTitle
        , feedAuthor      = blogAuthor
        , feedLinkHome = HomeR
        , feedLinkSelf = selfUrl
        , feedLanguage = "en-us"
        , feedUpdated = now
        , feedEntries = []
        }

getRssTagR :: Tag -> Handler RepRss
getRssTagR tag = rssFeed =<< taggedFeed (RssTagR tag) tag

getAtomTagR :: Tag -> Handler RepAtom
getAtomTagR tag = atomFeed =<< taggedFeed (AtomTagR tag) tag

main :: IO ()
main = do
    app <- toWaiAppPlain $ StaticPages undefined
    let renderPages = renderStaticPages app "site/"
    renderPages staticPageRoutePaths 
    postPaths <- map dropExtension `fmap` getPostFiles
    renderPages $ map toTextIgnore postPaths
    -- tagsPaths <- undefined

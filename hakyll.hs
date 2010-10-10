-- uploading -- can I add a command to hakyll for ./hakyll upload ?
-- cd _site && rsync -av --delete./  www-data@gregweber.info:/var/www/blog.gregweber.info/
module Main where

import Control.Arrow ((>>>))
import Control.Monad (when)

import Text.Hakyll
import Text.Hakyll.HakyllMonad
import Text.Hakyll.Render
import Text.Hakyll.Tags (readTagMap, renderTagCloud, renderTagLinks, withTagMap)
import Text.Hakyll.Feed (FeedConfiguration (..), renderRss, renderAtom)
import Text.Hakyll.File (getRecursiveContents, directory, removeSpaces)
import Text.Hakyll.CreateContext (createPage, createCustomPage, createListing)
import Text.Hakyll.ContextManipulations (renderDate, copyValue)

import Data.List (sort)
import Data.Map (toList)
import Control.Monad (forM_, liftM)
import Data.Either (Either(..))
import Data.Char (toUpper)

blogTitle = "Greg Weber's Programming Blog"
blogAuthor = "Greg Weber"

myConfig :: HakyllConfiguration
myConfig = (defaultHakyllConfiguration "http://blog.gregweber.info")
    { enableNoHtmlUrl = True }

main = hakyllWithConfiguration myConfig $ do
    directory css "css" -- Static directory.

    postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
    let renderablePosts = map ((>>> postManipulation) . createPage) postPaths

    let renderSite template = renderChain (template:["templates/default.html.hamlet"]) . withSidebar
        withSidebar = flip combine $ do
          let list = createPostListing "dummy" (take 3 renderablePosts) [("title", Left "Recent Posts")]
          let sidebar = renderAndConcat ["sidebar.html.hamlet"] [list]
          createCustomPage "dummy" [("sidebar", Right sidebar), ("blogTitle", Left blogTitle)]

        renderPostList url title posts = do -- TODO: paginate
          let list = createPostListing url posts [("title", Left title)]
          renderSite "posts.html.hamlet" list

    let tagMap = readTagMap "postTags" postPaths

    -- Render all posts
    renderPostList "posts.html" "All posts" renderablePosts
    
    -- Render post list per tag
    let renderListForTag tag posts =
            renderPostList (tagToUrl tag) ("Posts tagged " ++ tag)
                           (map (>>> postManipulation) posts)
    withTagMap tagMap renderListForTag

    -- Render index, including recent posts.
    let tagCloud = tagMap >>> renderTagCloud tagToUrl 100 200
        index = createPostListing "index.html"
                              (take 3 renderablePosts)
                              [ ("title", Left "Home")
                              , ("tagcloud", Right tagCloud)
                              ]
    renderSite "index.html.hamlet" index

    -- Render each post.
    forM_ renderablePosts $ renderSite "templates/post.html.hamlet"

    -- rss feed for all posts and tagged posts
    renderFeeds "all" (take 3 renderablePosts)
    withTagMap tagMap $ \tag posts ->
      when (tag `elem` ["haskell"]) $ renderFeeds tag (take 3 posts)

  where
    postManipulation =   renderDate "date" "%B %e, %Y" "Date unknown"
                     >>> renderTagLinks tagToUrl 

    tagToUrl tag = "$root/tags/" ++ removeSpaces tag ++ ".html"

    createPostListing url posts values =
        createListing url ["templates/postitem.html.hamlet"] posts values

    renderFeeds tag posts = do
      renderRss  (feedConfiguration "rss" tag) $ map postWithDescription posts
      renderAtom (feedConfiguration "atom" tag) $ map postWithDescription posts

    postWithDescription = (>>> copyValue "body" "description")

    feedConfiguration format tag = FeedConfiguration
        { feedUrl         = tag ++ "." ++ format
        , feedTitle       = (capitalize tag) ++ " Posts"
        , feedDescription = (capitalize tag) ++ " posts from " ++ blogTitle
        , feedAuthorName  = blogAuthor
        }

    capitalize (c:cs) = (toUpper c):cs

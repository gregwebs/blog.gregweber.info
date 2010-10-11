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

t template = "templates/" ++ template ++ ".html.hamlet"

main = hakyllWithConfiguration myConfig $ do
    directory css "css" -- Static directory.
    postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"

    -- every page renders through here
    -- put the "default" template at the base of render chain and add the sidebar
    let renderSite template = renderChain (template:[t "default"]) . withSidebar
        withSidebar = flip combine $ do
          let sidebarPosts = map ((>>> postSidebar) . createPage) postPaths
              list = createPostListing "dummy" (take 3 sidebarPosts) [("title", Left "Recent Posts")]
              sidebar = renderAndConcat [t "sidebar"] [list]
          createCustomPage "dummy" [("sidebar", Right sidebar), ("blogTitle", Left blogTitle)]

    let renderablePosts = map ((>>> postManipulation) . createPage) postPaths
        renderPostList url title posts = do
          let list = createPostListing url posts [("title", Left title)]
          renderSite (t "posts") list
    -- render each post and /posts
    forM_ renderablePosts $ renderSite (t "post")
    renderPostList "posts.html" "All posts" renderablePosts

    let tagMap = readTagMap "postTags" postPaths
        -- render tags/tag for each tag
        renderListForTag tag posts =
            renderPostList (tagToUrl tag) ("Posts tagged " ++ tag)
                           (map (>>> postManipulation) posts)
    withTagMap tagMap renderListForTag

    -- render index with post listing and tag cloud
    let tagCloud = tagMap >>> renderTagCloud tagToUrl 100 200
        index = createListing "index.html" 
                              [t "post-preview"]
                              (take 3 renderablePosts)
                              [ ("title", Left "Home")
                              , ("tagcloud", Right tagCloud)
                              ]
    renderSite (t "index") index

    -- rss feed for all posts and tagged posts
    renderFeeds "all" (take 3 renderablePosts)
    withTagMap tagMap $ \tag posts ->
      when (tag `elem` ["haskell"]) $ renderFeeds tag (take 3 posts)

  where
    postSidebar = renderDate "date" "%b %e" "Date unknown"
    postManipulation =   renderDate "date" "%B %e, %Y" "Date unknown"
                     >>> renderTagLinks tagToUrl 

    tagToUrl tag = "$root/tags/" ++ removeSpaces tag ++ ".html"

    createPostListing url posts values =
        createListing url [t "post-item"] posts values

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

-- pagination when I create a second post (use module)
-- sidebar navigation
-- uploading
  -- cd _site && rsync -av --delete./  www-data@gregweber.info:/var/www/blog.gregweber.info/
  -- can I add a command to hakyll for ./hakyll upload ?
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

myConfig :: HakyllConfiguration
myConfig = (defaultHakyllConfiguration "http://blog.gregweber.info")
    -- { enableNoHtmlUrl = True }

main = hakyllWithConfiguration myConfig $ do
    -- Static directory.
    directory css "css"

    -- Find all post paths.
    postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
    let renderablePosts = map ((>>> postManipulation) . createPage) postPaths

    -- Read tag map.
    let tagMap = readTagMap "postTags" postPaths

    -- Render all posts list.
    renderPostList "posts.html" "All posts" renderablePosts

    -- Render post list per tag
    let renderListForTag tag posts =
            renderPostList (tagToUrl tag) ("Posts tagged " ++ tag)
                           (map (>>> postManipulation) posts)
    withTagMap tagMap renderListForTag

    -- Render index, including recent posts.
    let tagCloud = tagMap >>> renderTagCloud tagToUrl 100 200
        index = createListing "index.html"
                              ["templates/postitem.html.hamlet"]
                              (take 3 renderablePosts)
                              [ ("title", Left "Home")
                              , ("tagcloud", Right tagCloud)
                              ]
    renderChain ["index.html.hamlet", "templates/default.html.hamlet"] index

    -- Render all posts.
    forM_ renderablePosts $ renderChain [ "templates/post.html.hamlet"
                                        , "templates/default.html.hamlet"
                                        ]

    renderFeeds "all" renderablePosts
    withTagMap tagMap $ \tag posts ->
      when (tag == "haskell") $ renderFeeds tag posts

  where
    postManipulation =   renderDate "date" "%B %e, %Y" "Date unknown"
                     >>> renderTagLinks tagToUrl 

    tagToUrl tag = "$root/tags/" ++ removeSpaces tag ++ ".html"

    renderPostList url title posts = do
        let list = createListing url ["templates/postitem.html.hamlet"]
                                 posts [("title", Left title)]
        renderChain ["posts.html.hamlet", "templates/default.html.hamlet"] list

    postFields posts =
        map (>>> copyValue "body" "description") posts

    blogTitle = "Greg Weber's Blog"
    blogAuthor = "Greg Weber"

    renderFeeds tag posts = do
      renderRss  (feedConfiguration "xml"  tag) $ postFields (take 3 posts)
      renderAtom (feedConfiguration "atom" tag) $ postFields (take 3 posts)

    feedConfiguration format tag = FeedConfiguration
        { feedUrl         = tag ++ "." ++ format
        , feedTitle       = (capitalize tag) ++ " Posts"
        , feedDescription = (capitalize tag) ++ " posts from " ++ blogTitle
        , feedAuthorName  = blogAuthor
        }

    capitalize (c:cs) = (toUpper c):cs

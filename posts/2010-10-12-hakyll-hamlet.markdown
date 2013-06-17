---
title: hakyll static blog engine with hamlet
description: first blog.gregweber.info blog post. Explain how I am using the hakyll static blog engine with hamlet
tags: haskell
---

Welcome!
------

I am moving here from [blog.thoughtfolder.com](blog.thoughtfolder.com) which will redirect here in the future. Expect this site to contain mostly highly nerdy technical posts like the last. If you hate the colors of this page let me know- it is still a work in progress.

I wanted to try out some new things for the infrastructure. I will still be using a static blog generator, but I am trying out the haskell generator that is available called [hakyll](http://jaspervdj.be/hakyll/). I have been programming in Ruby every day for a few years now, so I look to other language to learn more. Using more tools written with them is one way to further that. I would still highly recommend [Rassmalog](rassmalog.rubyforge.org/), the Ruby static blog engine I was using, although other Ruby static site generators seemed to have won more popularity. As a rubyist, probably all of the ruby static site generators are much easier for me to configure/program. Learning to use arrows in hakyll has been a challenge for my brain.

Hakyll is actually a static site generator, but you can download a tutorial that has a full blog setup. I really like how the preview seems to just work while making changes.

~~~
ghc --make hakyll.hs && ./hakyll preview

Starting hakyll server on port 8000...
~~~

I change my post and refersh

~~~
GET /posts/2010-09-30-first-post.html HTTP/1.1 => HTTP/1.1 200 OK
GET /css/default.css HTTP/1.1 => HTTP/1.1 200 OK
Rendering _site/posts.html
Rendering _site/tags/meta.html
Rendering _site/index.html
Rendering _site/posts/2010-09-30-first-post.html
Rendering _site/rss.xml
~~~

One nice aspect is integration with [pandoc](http://johnmacfarlane.net/pandoc/README.html). I am already using the pandoc extended markdown features.

Using Hamlet
------------
One thing that helped with the decision for hakyll was hamlet support. You just need to change the file names given to `renderChain`, so that they end in `.hamlet`. To generate hamlet from the example blog html I used the ruby haml toolset (requires a ruby installation and `gem install haml hpricot`). Here is my shell converter (a little zsh specific). It get things most of the way there- then I just had to fix the doctype. My shell fu is not that good. I wish I had spent the time towards creating a real html2hamlet converter- I don't see why it would be that hard using something like TagSoup.

    for f (*.html templates/*.html) html2haml --no-erb  $f | sed 's/\($[a-zA-Z]\+\)/\1$/g' | sed 's/ => /=/g' | sed 's/, :/!/g' | sed 's/", "/"!"/' | sed 's/{:\(.*\)}/!\1/g' | sed 's/"\/$/"/'  >| $f.hamlet


Hakyll limitations
------------------

I really like the capabilities that hakyll brings- creating static
sites with all the logic in the configuration file and nothing more
than simple variable substitution in the templating.
I think that improvements can be made so that writing the site configuration can be easier.
Jasper Van der Jeugt, the author has been responsive about my patches and ideas to improve things, and is continuing to actively develop Hakyll.
My biggest complaint right now is that there isn't a simple way to render templates within other templates. For my sidebar I have to use "dummy" strings.

``` haskell
let withSidebar = flip combine $ do
  let list = createPostListing "dummy" (take 3 renderablePosts) [("title", Left "Recent Posts")]
      sidebar = renderAndConcat ["templates/sidebar.html.hamlet"] [list]
  createCustomPage "dummy" [("sidebar", Right sidebar)]
```

I then added even more to my sidebar and that motivated creating what I think are missing functions in hakyll.

``` haskell
pageToString template page = renderAndConcat [template] [page]

templateToString template substitutions = pageToString template $
    createCustomPage "dummy" $ map (\(a,b) -> (a, Right b)) $ substitutions
```

This gives a decent alist interface to rendering a template.

``` haskell
let sidebar = templateToString "templates/sidebar.html.hamlet" [
                  ("recentPosts", recentPosts)
                , ("menu", menu)
                ]
```

My config file now violates the Hakyll philosophy that a site configuration should be < 100 lines.
I could envision haskellers writing their blogs and other simple sites in Hakyll and sharing their configuraiton file settings, much like in Xmonad.
Something like the Xmonad.Contrib could keep configuration file sizes down in size.
But I think hakyll is a little difficult to approach for those new to arrows like me, so I doubt it will ever gain a large user base.

My blog source is on github for anyone that wants to see my [config file](http://github.com/gregwebs/blog.gregweber.info/blob/master/hakyll.hs).

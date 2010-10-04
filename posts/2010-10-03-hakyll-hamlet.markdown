---
title: hakyll static blog engine with hamlet
description: The first blog.gregweber.info blog post. Explain how I am using the hakyll static blog engine with hamlet
tags: haskell
---

Welcome!
------

I am moving here from [blog.thoughtfolder.com](blog.thoughtfolder.com) which I may redirect here in the future. Expect this site to contain mostly highly nerdy technical posts like the last.

I wanted to try out some new things for the infrastructure. I will still be using a static blog generator, but I am trying out the haskell generator that is available called [hakyll](http://jaspervdj.be/hakyll/). I have been programming in Ruby every day for a few years now, so I look to other language to learn more. Using more tools written with them is one way to further that. I would still highly recommend [Rassmalog](rassmalog.rubyforge.org/), the Ruby static blog engine I was using, although it other Ruby static site generators seemed to have won more popularity. As a rubyist, probably all of the ruby static site generators are much easier for me to configure/program. Learning to use arrows in hakyll has been a challenge for my brain.

Hakyll is actually a static site generator, but you can download a tutorial that has a full blog setup. I really like how the preview seems to just work while making changes, although I am wary that things might slow down when I accumulate a lot of posts. There is already some experimentation on github with changes to use multiple cores though.

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

One nice aspect is integration with pandoc. I am already using the pandoc extended markdown features.

Using Hamlet
------------
One thing that helped with the decision for hakyll was hamlet support. I didn't see any explicit documentation for this feature. It turns out you just need to change the file names given to `renderChain`, so `index.html` will now be `index.html.hamlet`. To generate hamlet from the given blog html I used the ruby haml toolset (requires a ruby installation and `gem install haml hpricot`). Here is my shell converter (a little zsh specific). It get things most of the way there- then I just had to fix the doctype. My shell fu is not that good. I wish I had spent the time towards creating a real html2hamlet converter- I don't see why it would be that hard using something like TagSoup.

    for f (*.html templates/*.html) html2haml --no-erb  $f | sed 's/\($[a-zA-Z]\+\)/\1$/g' | sed 's/ => /=/g' | sed 's/, :/!/g' | sed 's/", "/"!"/' | sed 's/{:\(.*\)}/!\1/g' | sed 's/"\/$/"/'  >| $f.hamlet

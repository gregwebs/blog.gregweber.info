---
title: "Haskell sphinx client Upgraded"
description: "Haskell sphinx client upgraded to sphinx version 1.1 and new features implemented"
tags: "haskell"
---

I upgraded the Haskell implementation of a [sphinx full text search](http://sphinxsearch.com) client and put it
[on Hackage](http://hackage.haskell.org/package/sphinx). It is compatible with sphinx version 1.1.
New features were added- most of the full text searching options and abilities are now supported.
I also added the buildExcerpts command which can provide highlighted search results.

Sphinx is a very fast, and feature rich full-text search daemon. Written in C, people have reported it to be more performant than the Java option Lucene. There are other search engines out there, but none of them are as featurefull to my knowledge. IndexTank is a hosted search engine service that seems to be popular with those that don't want to administer a search service. Sphinx runs a daemon that will return search results to a client that your application uses.

Sphinx now has beta releases of version 1.1. Version 1.1 also supports the Sphinx Query Language, and it may be the future of sphinx clients. So there is an interesting opportunity going forward to use this. AFAIK SphinxQL requires a MySQL install, so I think it will always be nice to have an option that doesn't require that. Version 1.1 introduces real-time storage with sphinx- a very exciting possibility, but it is only supported through SphinxQL.

Thanks to Chris Eisdorf and Tupil for providing the original version of the Sphinx client. It has been very pleasant to extend their excellent work. He has handed over maintenance of the package to me.

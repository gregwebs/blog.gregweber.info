---
title: xss-sanitize - secure your html
description: on creating my first haskell library which sanitizes html
tags: haskell
---

Putting together a haskell library for XSS sanitization
=======================================================
Motivation
----------
I want a productive platform for developing web applications. Why haskell? One of the primary reasons is type safety. Type safety in an individual function is great. But what is impressive is building upon that to provide high-level guarantees. The [Yesod web framework](docs.yesodweb.com) can make guarantees that URLs exist and that database queries are valid among other things. But what is very important for web application developers is making security guarantees. Using Yesod's persistent library guarantess that SQL injection can't occur. For haskell to provide an attractive web applicaiton platform, we need to make as many security guarantees as we can at compile time. It is disapointing to me to build something with a safe language if the end result is still going to be something insecure. I saw a gaping securtiy hole- user supplied html content, and set out to banish it.

XSS attacks
-----------
Normally we know that the HTML pages we create are secure because we know exactly what is going to be displayed. The situation becomes different if we allow the user to give us html that we then display. We no longer know what will be displayed on the page, and can no longer be certain that it is secure. The html could now conatin javascript that could steal another user's cookie (particularly that of an admin).

Escaping vs. sanitizing
-----------------------
Many people are under the impression that we must choose between displaying (potentially insecure) html or escaping the html. Instead we can sanitize the html and we can now again be certain that only secure html is being displayed.

Building the library
--------------------
Apparently a lot of people have tried (and failed) to solve this problem with regular expressions. I know better than that. I have only tried to apply a regex to html a few times, and every time I have seen it done the result has has been ugly.

So the first thing to do was to figure out how to parse HTML. Writing modern software is all about standing on the shoulders of giants, so lets see what options exist.

Parsing HTML in haskell
----------------------
* Text.XML.HaXml.Html.Parsec (htmlParse)
* the parser from pandoc
* libxml2 at least for parsing - this parses "real world" html
* Text.XML.HXT.Parser.TagSoup (parseHtmlTagSoup)
* TagSoup parseTags/renderTags 

The foundation of parsing in Haskell is usually Parsec. John Macfarlane, the author of Pandoc, tole me that he wrote his own Parsec parser because Text.XML.HaXml.Html.Parsec wasn't flexible enough at the time he looked at it. The pandoc parser is theoretically a good html parser, but isn't broken out into a separate package.

Of course, we can always bind to C libraries. The C library of choice seems to be [libxml2](), which can quickly parse html into a manipulatable document. [Nokogirie]() is a library for the Ruby language which binds to libxml2 and allows HTML to easily be manipulated with css selectors or xpath. Haskell's multiple libxml2 bindings are low-level and incomplete.

TagSoup is a pure Haskell HTML parser. It has some limitations- parsing and re-rendering will produce a semantically equivalent result, but there are some limitations with quoting and self-closing tags. TagSoup also doesn't build up a document structure. However, there is Text.XML.HXT.Parser.TagSoup to build up a document structure.

It would be great if haskell could come up with a complete html manipulation solution- one that allows easy html manipulation without any limitations. This would probably come from the form of complete libxml2 bindings with a high-level interface or from improvments to TagSoup to remove its limitations.

When I first embarked upon this project, I told Michael Snoyman, the author of Yesod, and he told me Pandoc had an html sanitizer, but that TagSoup would work well for parsing html. So after spending hours looking into alternatives I took the html sanitization code from Pandoc and hooked up the TagSoup library and was able to quickly have a first version of a sanitizer.

White-lists
-----------
All the parsed html must be ran through a white-list of acceptable elements and attributes, and removed if they don't match.

The only problem at this point is that I was trusting the Pandoc implementation. So I looked over html5lib project and revised my white lists accordingly. html5lib filters css with regular expressions- something I was not comfortable with porting until carefully studying. It also has a few more special cases that I haven't bothered with yet.

The Result
==========
A library, [xss-sanitize](http://github.com/gregwebs/xss-sanitize), with one function, [sanitizeXSS](hackage.haskell.org/) that sanitizes html. The most powerful way to leverage this is to filter any trusted html immediately upon receiving it (instead of waiting until right before it will be displayed).

In the Yesod framework the data for an HTML data type will automaticaaly be sanitized when it is read from form data. XSS attacks have been banished from Yesod. Lets banish them from haskell completely.

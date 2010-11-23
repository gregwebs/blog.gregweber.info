---
title: Controlling a child proces in linux
description: Creating an automatic build process
tags: haskell linux
---

Motivation
==========
I wanted my [Yesod](http://docs.yesodweb.com) application to automatically build when I make changes.

Watch directories, not files. Filter incoming modifications to simulate watching a file.
Watching a directory is not recursive.

Result
======

    ./hinotify ./ *~dist(/)

That is ZSH syntax. That is a normal star shell glob but then we are removing dist from the glob. (/) means only directories.

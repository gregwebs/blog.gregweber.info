---
title: Automatic project building in haskell
description: Struggling with automatically managing re-building of files as they are changed
tags: haskell,linux
---


I wanted my [Yesod](http://docs.yesodweb.com) application to automatically respond appropriately to file changes. The code needs to be rebuild. I am using [coffescript](http://jashkenas.github.com/coffee-script/) which needs to be converted to JavaScript, I want tests to be automatically run. These kinds of things help keep me in flow instead of worrying about the state of my application.

I am still figuring out how to accomplish this in Haskell. One problem seems to be- how can a child process be properly controlled- started, run, and restarted? fork + exec is the only think I could get working without having to deal with "zombie processes"

~~~~~~~~~~~~~~~~~~~~ {.haskell}
  run cmd args = executeFile cmd True args Nothing

  pidVar <- newEmptyMVar

  let killProcess = do
        pidExisting <- takeMVar pidVar
        rawSystem "kill" ["-9", show pidExisting]
        -- this calls wait, which prevents the child from being a zombie
        _ <- getProcessStatus True True pidExisting
        return ()

  let runProcess command args = do
        pidNew <- forkProcess $ run command args >> return ()
        putMVar pidVar pidNew

~~~~~~~~~~~~~~~~~~~~

This seems to be working well enough. But it is limited- there isn't a direct way to execute a shell command made of multiple commands like: `cabal build && ./run`

I am using the [hinotify](http://hackage.haskell.org/package/hinotify) library, and I think I finally have that figured out. It works best when I watch directories, not files and filter the incoming modification events to restrict them to certain files (by file extension). One big thing to remember is that watching a directory does not watch its subdirectories.

I kick the watcher off with this command that gives it the directories to watch- this shows off why I use Z Shell. It is easy to remove things from a star glob with `~` (the dist directory in this case) and to restrict a glob to only directories with `(/)`

~~~~~~~~~~~~~~~~~~~~
#!/usr/bin/zsh
setopt extendedglob
ghc --make hinotify.hs && ./hinotify ./ **/*~dist*(/)
~~~~~~~~~~~~~~~~~~~~

I would like to turn this effort into something easy to re-use, but I am still not happy with the situation. And of course this only works on linux. I would really like to be using a childProcess library and a file system watcher library that were a little higher level, and ideally worked cross-platform.

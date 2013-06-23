---
title: "Haskell Error Location"
description: "A Template Haskell version of error that tells you where the error is from"
tags: haskell
---

One of the most annoying things about Haskell is a lack of any useful information on a crash. This isn't as bad as it sounds because it actually is not hard to write a program that does not crash due to the excellent type safety. But sometimes I just want a quick and dirty hack to assert that my program is sane and I don't care if it crashes. One case of this would be supporting code that is only executed for tests. Rather than having to check for an error return type, I might rather write a partial function that always returns a success type and will otherwise fail.

Template Haskell exposes a little known location data structure so that you can know exactly where the template haskell is being invoked, so my version of error that gives the file location looks about like this:


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax

err :: String -> Q Exp
err str = do
    loc <- qLocation
    let prefix = (location loc) ++ " "
    [|error (prefix ++ str)|]
  where
    location loc = (loc_package loc) ++ ":" ++ (loc_module loc) ++ " " ++
      (loc_filename loc) ++ ":" ++ (line loc) ++ ":" ++ (char loc)
    line = show . fst . loc_start
    char = show . snd . loc_start

> $(err "OH NO!")
main:Main main.hs:16:1 OH NO!
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


I put a module up on Hackage called [ErrorLocation](). It exposes the functions err (error), undef (undefined), and trc (Debug.Trace.trace). All of these behave the same as their normal counterpart but also spit out a location. I also included my favorite helper, debug, which is like trace but just show the value.


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
debug :: Show a => a -> a
debug x = trace ("DEBUG: " ++ show x) x

> debug [1,2,3]
DEBUG: [1,2,3]
[1,2,3]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


I am going to need a little help for the Template Haskell version of that one, but then I will include it also.

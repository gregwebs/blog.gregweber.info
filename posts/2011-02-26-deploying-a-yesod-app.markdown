---
title: Deploying haskell to amazon
description: Deploying a haskell web application to the Amazon cloud.
tags: haskell,linux,amazon
---

The haskell web development community is [discussing deployment options](http://www.haskell.org/pipermail/web-devel/2011/001095.html).
I though I would document how I deploy [my Yesod web application](http://eatnutrients.com).

Web development in haskell is just now taking off- a lot of things are immature or even virgin territory.
One aspect of this is web application deployment- there are no haskell deployment tools I am aware of.
Perhaps the biggest obstacle to this is the lack of even an ssh library.
Given that there are thousands of haskell libraries available now, this was a suprising realization!

As bad as this may sound, the community is following through in the right order- first create great web frameworks, then make them easy to deploy.
To use Ruby on Rails as an example, when it first came out deployment options were horrible.
But the framework got popular, so people created solutions, and even businesses around deploying Rails applications.
Today you can deploy a rails application to [Heroku](http://heroku.com/) without thinking about deployment hassles.
Haskell is actually starting from a much better position- all of the haskell frameworks already have fast and highly scalable web servers, the fastest being [Warp](http://docs.yesodweb.com/blog/preliminary-warp-cross-language-benchmarks).
This was a problem in the Ruby community for a long time, along with the high memory usage of Ruby and its other limitations (GIL, blocking IO, single core).
 All of these hard problems are solved in haskell. Now we just need some deployment techniques.

Perhaps we don't need a solution that is very haskell specific.
I first tried out one of the most talked up modern Ruby deployment tools- Chef.
I understand that this tool is wonderful for some organizations, but for my very simple needs it was not a good fit.
The documentation was horrible even though you are supposed to always be using their API and their abstraction techniques.
It also requires a Ruby install on the server.
The second Ruby tool I tried out was Rudy, an amazon specific ssh deployment tool.
I liked it a lot, but at a certain point it just stopped working for me, and after talking with the author and spending a lot of time trying to fix it, I just gave up, and then thought- why not make my own haskell tool?

Haskell does have some shell libraries. The 2 main libraries I saw were [HSH](http://hackage.haskell.org/package/HSH) and [Shellish](http://hackage.haskell.org/package/shellish).
HSH seemed to focus on 2 things I didn't care about- piping between haskell and the shell, and polymorphic return values from shell calls.
So I decided to go with Shellish and have been happy with my decision.

I decided to deploy to amazon. Amazon is a very attractive solution because they offer a free micro server for starting your application, it is relatively easy to scale that server up to a larger instance or to scale out, and you can reduce your network latency by deploying to multiple data centers around the world.

My deployment situation leverages the fact that my development computer is the same as production- 64 bit Ubuntu- so I can make binaries on my local computer. I highly recommend this strategy- if I had a 32 bit computer I would deploy to a 32 bit amazon image. Compiling, particularly linking, on a micro amazon instance is difficult. My strategy is binaries *only* on the micro instance, no compiling. For compiling elsewhere, I would spin up a staging server on amazon to compile and test my applications, have a fast transfer to the micro instance, and then shut the server down. Here is how some [compile ghc on amazon](http://hackage.haskell.org/trac/ghc/wiki/AmazonEC2).

I have been so used to Ruby that I didn't realize how nice it is to deploy a single binary and just run it.
I compile my [Yesod](http://docs.yesodweb.com) application locally with a production flag: `cabal configure -fproduction && cabal build`.
Then I rsync it to the production server, along with a few other files like static assets that are not compiled into the binary.
The main pain point of this is rsyncing the binary- transferring within amazon from a staging server could make this much quicker.
Finally I ssh to the production server and restart the application with the new binary.

Installing server infrastructure is also done with Shellish.
Basically I am just running shell commands from haskell.
This lets me build up a little bit of safety and convenience in the haskell code.
I try to make all the installs idempotent- they check to see if the install has already happened.

~~~~~~~~~~~~~~~~~~~~ {.haskell}
command :: String -> [String] -> [String] -> ShIO String
command com args more_args =
  run com (args ++ more_args)

rsync   = command "rsync" ["--delete", "-avz", "--rsh", "ssh -i " ++ keyfile]

uploads :: [FilePath] -> ShIO String
uploads locals = rsync $ ["--relative"] ++ locals ++ [login]

apt_get mode more = sudo "apt-get" (["-y", "-q", mode] ++ more)

install = do
  apt_get "install" ["mongodb-stable"]
~~~~~~~~~~~~~~~~~~~~

Monitoring your application is an important part of a deployment.
I am happily using Monit for that task. One down-side of monit is that it requires a pidfile for each process.
So I wrote a [wrapper script](https://gist.github.com/889277) that can launch my web application and create a pidfile.
My .monitrc file:

~~~~~~~~~~~~~~~~~~~~
check process app-production with pidfile /home/ubuntu/app/tmp/app-production.pid
  start program = "/home/ubuntu/app/script/pid-wrap start ./script/app-production"
  stop program  = "/home/ubuntu/app/script/pid-wrap stop  ./script/app-production"

set mailserver smtp.gmail.com port 587 username <USERNAME>
 password <PASSWORD> using tlsv1 with timeout 30 seconds

set alert <EMAIL>
set mail-format {
    from: <APP-EMAIL>
    subject: [MONIT]
  }
~~~~~~~~~~~~~~~~~~~~

There are definitely improvements to be made to the deployment process.
I can't rollback deployments, and ideally I would have a zero downtime transition.
I will probably end up going back to a more mature (non-haskell) deployment tool.
I am considering one I recently discovered that has amazon integration- [Rubber](https://github.com/wr0ngway/rubber/wiki/Quick-Start).
But I may still keep using my haskell shell wrappers with such a solution.

I want to encourage the haskell community to share their deployment techniques.
When I come up with something I am happy with I will put it on [the wiki](http://www.haskell.org/haskellwiki/Web/Deploy).
Hopefully we can come up with a step-by-step deployment tutorial.

There is also a much more ambitious endeavor to create a Heroku-like service for haskell web applications that is [also being discussed](http://www.haskell.org/pipermail/web-devel/2011/001095.html). This would be a great tool for the haskell community, but custom deployment techniques will always be needed for more complex use cases.

---
title: High Performance Ruby Part 3: non-blocking IO and web application scalability
description: Leveraging non-blocking IO to scale to thousands of simultaneous requests with evented web servers
tags: ruby,fibers,io
---

This is part 3 of a series. [Part 1](/posts/2011-06-03-high-performance-rb-part1) was on fibers, enumerators and scalable XML parsing, and shows other benefits to fibers.


# Ruby web application performance

Ruby on Rails is a web framework that if anything has shown that programmer productivity is normally much more valuable than web application performance. Rails is one of the worst performing options available (although roughly tied with some other dynamic language frameworks). But for many applications, its performance is fine. And there are always caching layers that can be added to make a web application faster. In addition, it is always conceptually simple to scale a web *application*- just get more application servers- the difficult bottlenecks often come down to the database.

Yet the performance issues still translate to more complicated deployment setups for even the simplest of applications. What I am hoping the Ruby community can achieve is the same ease of programming of Rails, but with easier deployment and *much* better scalability. But lets step back and look at the situation we are in.


# CPU bound vs IO bound

IO bound means your program doesn't do anything while it waits for network or disk access.
If your application is CPU bound, this article doesn't have much to offer you. The good news is that non-blocking IO will not hurt you in that case, even if it doesn't help you.


# Web Server deployment architectures

The Ruby community has always insisted that performance is not an issue while constantly searching for higher performance web servers and application stacks. Some of the performance issues of Ruby had to do with the inherent slowness of the original 1.8 MRI Ruby interpreter. But performance issues have also always been related to sub-optimal deployment and framework architectures.

  Server Architecture Server
  ------------------- --------------------------------------------
  Threaded            Mongrel, JRuby (Torquebox or other options)
  Fork                Phusion Passenger, Unicorn
  Evented             Thin, Goliath


  Ruby implementation GIL Fibers Threads
  ------------------- --- ------ --------------------------------------------
  MRI 1.8             Y   N      Green
  MRI 1.9             Y   Y      OS
  JRuby               N   slow   Java Threads (OS threads)
  Rubinius 1.x        Y   N      Green, OS
  Rubinius 2.0        N   Y      OS


## The original simple solution 

Mongrel or Webrick do little more than give your web app a server interface. Mongrel was actually threaded, but MRI 1.8 was never able to take advantage of that and Rails wasn't even thread safe when the community was using Mongrel.
With these servers, Rails could only handle one request at a time. To handle four requests per second one had to launch four application instances, bloating memory usage and complicating deployment.


## Threading and JRuby deployment options

Rails is now thread safe, and JRuby can take advantage of this by running a Rails application on a multi-core machine using Java threads. JRuby also has an entirely different set of Java deployment options. See the end of this post for an explanation of Java deployment architecture. Here is [a benchmark](http://torquebox.org/news/2011/02/23/benchmarking-torquebox/) demonstrating that JRuby deployment options can have superior performance characteristics to most MRI deployment techniques used today. Also note that this provides a valid comparison between unicorn/passenger/Thin.

But the situation is not as rosy in MRI. In MRI 1.8 because Ruby threads were green threads that could not take advantage of multi-core. Even though one can create OS threads in 1.9, the [GIL still means this isn't a realistic option](http://www.igvita.com/2008/11/13/concurrency-is-a-myth-in-ruby/).


## Forking

Unicorn and Phusion Passenger get around Ruby's inherit concurrency weaknesses by using fork. Fork is an OS capability that allows one to clone a process, but for the memory to be copy-on-write. Copy-on-write means that forked processes can share memory until one of the processes writes to memory, at which point just the written memory will be copied. This results in dramatically lower memory usage than just creating a duplicate process. Each fork is a separate OS process and thus can be scheduled on a separate core, achieving concurrency while limiting memory bloat. And Unicorn and Passenger both do an excellent job of managing the forked processes, including zero downtime restarts. Passenger is a module for Nginx or Apache, so it is a nice option if you are already deploying one of those web servers. [Passenger Nginx may perform a bit better than Passenger Apache](http://snaprails.tumblr.com/post/444462071/passenger-benchmark-on-apache-nginx). Unicorn, on the other hand is its own forking web server.

Unfortunately, the only Ruby garbage collector that is copy on write friendly is REE, which is the default version of Ruby used in Passenger. [This article has a good overview of Ruby concurrency](http://essenceandartifact.blogspot.com/2011/09/sad-state-of-concurrency-in-ruby-192.html). Normal Ruby garbage collectors are not copy on write friendly because they update objects when they run. So you don't end up with as much memory savings, although it still can be a convenient deployment strategy.

## Threading/Forking and blocking requests

When a request comes in to a web application it will block while trying to access the database instead of allowing other requests. Therefore a Rails application can only handle one request at a time or one request per OS thread/fork. This shows a potential solution to blocking IO- create another fork or OS thread! In practice, forks appear to be too expensive of a mechanism to take this concept very far at all. Threads are a viable option in JRuby and Rubinius 2.0. An OS can have an easy time scheduling hundreds of threads, making OS threads a possible solution, even if they are a bit heavy-weight (context switches are expensive when changing OS threads compared to more light-weight threads).

We already discuseed how MRI's GIL prevents threads from working across multi-core. But it is actually possible to use threads to [achieve asynchronous IO](http://yehudakatz.com/2010/08/14/threads-in-ruby-enough-already/) on a single core in Ruby. For standard use of IO, Ruby will suspend the current thread and let another one run. The problem is that badly behaving native extentions (like the original MySQL driver) will prevent this from happening. So you can achieve async IO with well behaved native extentions and one application instance per core (whereas with JRuby you only need one application instance per computer). Although I don't view them as the ideal solution because they are more expensive than application threads, OS threads are a reasonable approach to concurrency for most web applications which can benefit from asynchronous IO and don't want to deal with the hassles of the evented option. They may be easier to incorporate with existing code which uses blocking IO. Rails has actually supported this for a while, but it has always been disabled by default.


## Evented

Often times evented and threaded are thought of as diametrically opposed. However, a good non-blocking IO implementation can blur the lines and it is possible to view evented through the lens of threads. For each request that comes in, we want to treat it as a separate thread of execution, and pause that execution when there is IO so that other threads of execution can run. And when the IO completes, that is an event that will wake up our paused thread of execution.

The evented model gets around the limitations of non-blocking IO. Evented servers scale amazingly well and represent the future of Ruby. The only current downside that is an inability to scale across multiple cores. Right now you have to run an evented Ruby process for each core.

[Goliath](https://github.com/postrank-labs/goliath) was [recently released](http://www.igvita.com/2011/03/08/goliath-non-blocking-ruby-19-web-server/) for Ruby, with the promise of great performance. Goliath is a fully evented server and web framework combined that focuses on non-blocking IO.

The alternative to the Goliath server is Thin. Thin is an evented Ruby web server used in production today. Thin has been around for a while now, but hasn't delivered a huge win because of the prevalence of blocking IO in Ruby. Thin can be used with any Rack based application.


## non-blocking IO and fibers

Just because your evented web server can handle thousands of requests per second still doesn't mean your app can handle more than a few requests per second. The culprit may be blocking IO. When there is a call to the database, the application will just wait for the response, performing no useful work and not taking on another request.

The solution is to use non-blocking IO so that another thread of execution can work while one is waiting on IO. node.js is well known for doing this. However, the callback style used by node.js is much more difficult to write, maintain, and reason about, particularly when there are multiple IO actions in one request. Instead of a simple, synchronous flow, you have to have a callback for every IO action. Ruby was always fundamentally capable of non-blocking IO, especially after EventMachine was released, but that style of programming was never appealing.

The amazing opportunity with Ruby 1.9 is to use Ruby's fibers to abstract away the callbacks and just write normal code that looks synchronous. Each IO action is performed inside a fiber that is paused during the IO, allowing other fibers to run. When the IO is finished, the fiber can be resumed. And this style is actually not that hard to manage- it only has to be done once by the library writer- the implementation details become transparent to the library user.

Ruby's fibers are not as performant on jRuby or Rubinius, so non-blocking IO is best used right now with MRI 1.9. A new version of Rubinius was released that does not have a GIL, meaning that once Rubinius's fibers mature, the evented model will be capable of scaling to multi-core.

## Putting together evented and nob-blocking for high scalability

Ruby's evented and non-blocking landscape actually make it easy to have a non-blocking web application. Basically you need to use an evented web server/framework (Goliath or Sinatra-Synchrony with Thin) and find the non-blocking version of the driver for your database. 


## non-blocking (asynchronous) frameworks

  Server  Framework
  ------  ----------
  Goliath Goliath
  Thin    Sinatra
  Thin    Rails

The ideas is not entirely new- [Cramp](http://cramp.in) has been around for a while, however its initial version was not that compelling and not many in the Ruby community noticed it. Recently a new version has been released that focuses on streaming (WebSockets, Server-Sent Events). Using cramp is not an exclusive option- it is best for streaming actions of an application and other actions can be handled with a different framework.

There is an exciting new [async Sinatra project](https://github.com/kyledrake/sinatra-synchrony) that appears to integrate seamlessly with Sinatra. In [some very simple benchmarks it performed very well](https://gist.github.com/999390). This [older async Sinatra](https://github.com/raggi/async_sinatra) project seems to have a little rougher integration.

There is no fundamental reason I know of why these techniques can't be applied to Rails. There is a [demo async Rails setup](https://github.com/igrigorik/async-rails).

Rubyists can also use Goliath, an asynchronous framework released by [Ilya Grigorik](www.igvita.com/), who has been very involved in pushing the asynchronous concept in the Ruby community.

### Goliath

Although Goliath is not featureful, it is an async solution that I feel I can trust because PostRank has been using it in production for a long time. Its low-level nature means we can hope for speed closer to something like Rack than Sinatra. It also means most Goliath application code should be fairly easy to port to any other Ruby framework because it is low-level Rack compatible code. The future of Goliath is a little in question now- I am happy that Postrank, the developers of Goliath, got bought by Google, but Google does not use Ruby. However, since that announcement the Goliath community has stayed active and released a new version.

Goliath It is still a bit immature. Luckily code base is nice and I was able to submit a few patches. And the community seems to be gradually addressing Goliath's weaknesses. Here are some specifi pain points:

  * configuration is a bit odd and difficult to load outside of Goliath
  * middleware
    - working with existing rack tools does not always work
    - There is no halt like in Sinatra or around filters like in Rails- you are forced to put certain abstraction into middleware (unless you want to try adding these kinds of filters yourself).

From the start I assumed that Goliath the server would be separated from Goliath the framework. However, I haven't seen any indication of interest in this from the Goliath maintainters. I would like to be able to write fast and simple code with Goliath, move to Sinatra when more features are needed, and move to Rails when many features are needed.

I wish I had gone the async Sinatra route so that I could leverage the conveniences of Sinatra, but that option was not available when I started with Goliath. I would definitely try this route (or the async rails route) for anything more than just an API (need to present a web interface to users). And for streaming capabilities Cramp appears to be a great option.

## The price of abstraction

A simple Rack application is always going to be faster at a microbenchmark than a framework layered on top of it. In [these benchmakrs](https://github.com/DAddYE/web-frameworks-benchmark/wiki/Achiu) Sinatra is often 2-3x slower than Rack, and Rails is often 3-4x slower than Sinatra. Goliath is in the style of low-level Rack code, so I would expect it to perform noticeably better than an asynchronous Sinatra solution.


## Ruby's future

MRI 1.9 is still king because of its fibers, but those fibers won't spread across cores. Rubinius has a great opportunity to help Ruby scale multi-core. Expect JRuby's fiber implementation to mature.


## Other languages alternatives.

Language   Non-blocking              Multi-core
-------- --------------------------- --------------------
Java     with OS threads or Akka lib with OS threads or Akka lib
JRuby    with OS threads             with OS threads
Ruby     non-blocking Fiber libs     Single core
Python   non-blocking libs           Single core
Node.js  non-blocking libs           Single core
Erlang   built into runtime          Multi core
Haskell  built into runtime          Multi core

Obviously there are going to be more possibilities here. I will speak to what I know a little about.

* Python - a similar situation as Ruby, with evented web frameworks like Twisted and Tornado on the rise.
* Node.js - async I/O, but you have to deal with it manually because it doesn't have fibers like Ruby. Really there is no reason to use Node.js anymore unless programming in javascript is such an imperative that you want to deal with the asynchronous flow. It does perform better than Goliath, but if you really need that you may as well use Erlang or Haskell.
* Java - relies purely on OS threads. So it can take on only as many simultaneous users with blocking database requests as the OS can schedule threads. 
* Java/JRuby - JRuby gets what Java has. Fiber support is mapped to Java (OS) threads.
* Java + [Akka](http://akka.io/) - Akka is a concurrency library for Java featuring Actors, STM, and fault tolerance, attempting to put Java at Erlang's level. It can be [integrated with JRuby](http://metaphysicaldeveloper.wordpress.com/2010/12/16/high-level-concurrency-with-jruby-and-akka-actors/).
* Erlang  - non-blocking I/O by default, runtime that scales actors to multi-core. Amazing distributed system capabilities (that your web application front-end probably doesn't need).
* Haskell - non-blocking I/O by default, runtime that scales cheap threads to multi-core. It is a compiled language capable of very fast execution. I am a contributor to the [Yesod web framework](http://yesodweb.com), which is [much faster](http://www.yesodweb.com/blog/preliminary-warp-cross-language-benchmarks) than dynamic languages or Java, and scales better than any web application server I know of.


## Conclusion

Use MRI 1.9 rather than MRI 1.8 on all new projects, and try to use asynchronous drivers for your database and any other IO. And see if you can make your Rails app asynchronous or try async-sinatra. We need to make non-blocking IO the default, at which point Ruby applications will have a much easier time scaling IO bound workloads. I know this isn't a magic bullet- you are still going to have to scale your database and make sure you don't blow up memory usage in your Ruby code. But it will translate to better performance, reduced memory consumption, and simpler deployments.

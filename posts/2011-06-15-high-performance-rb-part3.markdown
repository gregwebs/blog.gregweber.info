---
title: High Performance Ruby Part 3: non-blocking IO and web application scalability
description: Leveraging non-blocking IO to scale to thousands of simultaneos requests with evented web servers
tags: ruby,fibers,io
---

This is part 3 of a series. [Part 1](/posts/2011-06-03-high-performance-rb-part1) was on fibers, enumerators and scalbale XML parsing, and shows other benefits to fibers.

# Ruby web application performance

Ruby on Rails is a beautiful web framework that if anything has shown that progammer productivty is normally much more valuable than web application performance. Rails is one of the worst performing options available (although roughly tied with some other dynamic language frameworks). But for many applications, its performance is fine. And there are always caching layers that can be added to make a web application faster. In addition, it is always conceptually simple to scale a web *application*- just get more application servers- the difficult bottlenecks often come down to the database.

Yet the performance issues still translate to more complicated deployment setups for even the simplest of applications. What I am hoping the Ruby community can achieve is the same ease of programming of Rails, but with easier deployment and *much* better scalability. But lets step back and look at the situation we are in.

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
With thes servers, Rails could only handle one request at a time. To handle four requests per second one had to launch four application instances, bloating memory usage and complicating deployment.


## Threading and JRuby deployment options

Rails is now thread safe, and JRuby can take advantage of this by running a Rails app on a multi-core machine using Java threads. JRuby also has an entirely different set of Java deployment options. See the end of this post for an explanation of Java deploment architecture. Here is [a benchmark](http://torquebox.org/news/2011/02/23/benchmarking-torquebox/) demonstrating that JRuby deployment options can have superior performance characteristics to most deployment techniques used today. Also note that this provides a valid comparison between unicorn/passenger/Thin.

But the situation is not as rosy in MRI. In MRI 1.8 because Ruby threads were green threads that could not take advantage of multi-core. Event though one can create OS threads in 1.9, the [GIL still means this isn't a realistic option](http://www.igvita.com/2008/11/13/concurrency-is-a-myth-in-ruby/).

## Forking

Unicorn and Phusion Passenger get around Ruby's inherit concurrency weaknesses by using fork. Fork is an OS capability that allows one to clone a process, but for the memory to be copy-on-write (COW). COW means that a fork of another process doesn't technically require additional memory until one of the processes write to memory, at which point it will create a copy, minimizing memory usage. Each fork is a separate OS process and thus can be scheduled on a separate core, achieving concurrency while limiting memory bloat. And Unicorn and Passenger both do an excellent job of managing the forked processes, including zero downtime restarts. Passenger is a module for Nginx or Apache, so it is a nice option if you are already deploying one of those web servers. [Passenger Nginx may perform a bit better than Apache](http://snaprails.tumblr.com/post/444462071/passenger-benchmark-on-apache-nginx). Unicorn, on the other hand is its own web server.

## Threading/Forking and blocking requests

When a request comes in to a web application it will block while trying to access the database instead of allowing other requests. Therefore a Rails app can only handle one request at a time or one request per OS thread/fork. This shows a potential solution to blocking IO- create another fork or thread! In practice, forks appear to be too expensive of a mechanism to take this concept very far at all. And MRI's GIL means threads only work in JRuby (and the new version of Rubinius). An OS can have an easy time scheduling hundreds of threads, making OS threads a possible solution, even if they are a bit heavy-weight (context switches are expensive when changing threads).


## Evented

The evented model gets around the limitations of non-blocking IO, but currently can only handle multi-core by creating multiple processes. Evented servers scale amazingly well, and represent the future of Ruby.

[Goliath](https://github.com/postrank-labs/goliath) was [recenty released](http://www.igvita.com/2011/03/08/goliath-non-blocking-ruby-19-web-server/) for Ruby, with the promise of great performance. Goliath is a fully evented server and web framework combined that focuses on non-blocking IO.

The alternative to the Goliath server is Thin. Thin is an evented Ruby web server used in production today. Thin has been around for a while now, but hasn't delivered a huge win because of the prevalence of blocking IO in Ruby. Thin can be used with any Rack based application.


## non-blocking IO and fibers

Just because your evented web server can handle thousands of requests per second, still doesn't mean your app can handle more than one request at a time! The culprit is blocking IO. As soon as there is a call to the database, the application will wait for the response without taking on another request.

The solution is to use non-blocking IO so that another thread of execution can work while one is waiting on IO. node.js is well known for doing this. However, this style of coding is much more difficult to write, maintain, and reason about, particularly when there are multiple IO actions in one request. Instead of a simple, synchronous flow, you have to have a callback for every IO action. Ruby was always fundamentally capable of non-blocking IO, especially after EventMachine was released, but that style of programming was never appealing.

The amazing opportunity with Ruby 1.9 is to use Ruby's fibers to abstract away the callbacks and just write normal code that looks synchronous. Each IO action is performed inside a fiber that is paused during the IO, allowing other fibers to run. When the IO is finished, the fiber can be resumed. And this style is actually not that hard to manage- it only has to be done once by the library writer- the implementation details become transparent to the library user.

Ruby's fibers are not as performant on jRuby or Rubinius, so non-blocking IO is best used right now with MRI 1.9. A new version of Rubinius was released that does not have a GIL, meaning that once Rubinius's fibers mature, the evented model will be capable of scaling to multi-core.

## Putting together evented and nob-blocking for high scalability

Ruby's evented and non-blocking landscape actually make it easy to have a non-blocking web application. Basically you need to use an evented web server/framework (Goliath or Sinatra-Synchrony with Thin) and find the non-blocking version of the driver for your database. 


## non-blocking (asynchronous) framewoks

  Server  Framework
  ------  ----------
  Goliath Goliath
  Thin    Sinatra
  Thin    Rails

The ideas is not entirely new- [Cramp](https://github.com/lifo/cramp) has been around for a while. But it was createed before Ruby 1.9- it seems to encourage using its own ORM and has some rough spots around EM integrations.

There is an exciting new [async Sinatra project](https://github.com/kyledrake/sinatra-synchrony) that appears to integrate seamlessly with Sinatra. In [some very simple benchmarks it performed very well](https://gist.github.com/999390). This [older async Sinatra](https://github.com/raggi/async_sinatra) project also seems to have a little rougher integration.

There is no reason why these techniques can't be applied to Rails. There is a [demo async Rails setup](https://github.com/igrigorik/async-rails).

Goliath is a production ready asynchronous framework.

### Goliath

Although Goliath is not featureful, it is an async solution that I feel I can trust because PostRank has been using it in production for a long time. Its low-level nature means we can hope for speed closer to something like Rack than Sinatra. The future of Goliath is a little in question now- I am happy that Postrank, the developers of Goliath, got bought by Google, but Google does not use Ruby, so I would expect them to transition away from Ruby and become gradually more disengaged from Goliath. Fortunately there is a small but active and responsive community of users behind Goliath, and Goliath application code should be fairly easy to port to any other Ruby framework because it is fairly low-level. I expected Goliath to be low-level, but I do have some complaints.

  * It is still a bit immature. I have submitted several patches, but more issues probably await.
  * configuration is a bit odd and difficult to load outside of Goliath
  * middleware
    - working with existing rack tools does not always work
    - There is no halt like in Sinatra or around filters like in Rails- you are forced to put certain abstraction into middleware (unless you want to try adding around filters)- 

For a framework that encourages everything to be done in middleware, it was strange for many of us to find out that the middlewares do not have their own fiber yet! This means you can't contact a database for authentication, and can't rescue an exception. But there is already a fork on github of Goliath that fixes this- expect it to be merged back to the main repo.

I hope Goliath the server can be separated from the framework and be used for any async Rack framework. Then I can write fast code with Goliath, move to Sinatra when more features are needed, and move to Rails when many features are needed.

I wish I had gone the async Sinatra route, but that option was not available when I started with Goliath. *Definitely* try this route (or the async rails route) if you have anything more than just an API (need to present a web interface to users).

## The price of abstraction

A simple Rack application is always going to be faster at a microbenchmark than a framework layered on top of it. In [these benchmakrs](https://github.com/DAddYE/web-frameworks-benchmark/wiki/Achiu) Sinatra is often 2-3x slower than Rack, and Rails is often 3-4x slower than Sinatra. Goliath is really an asynchronous extension on top of Rack, so I would expect it to be able to perform 2x better than an asynchronous Sinatra solution.


## Ruby's future

MRI 1.9 is still king because of its fibers, but those fibers won't spread across cores. Rubinius has a great opportunity to help Ruby scale multi-core. Expect JRuby's fiber implementation to mature.


## Other languages alternatives.

Language   Non-blocking            Multi-core
--------- ------------------       --------------------
Java       with OS threads         with OS threads
JRuby      with OS threads         with OS threads
Ruby       non-blocking Fiber libs Single core
Python     non-blocking libs       Single core
Node.js    non-blocking libs       Single core
Erlang     built into runtime      Multi core
Haskell    built into runtime      Multi core

Obviously there are going to be more possibilities here. I will speak to what I know a little about.

* Python - a similar situation as Ruby, with evented web frameworks like Twisted and Tornado on the rise.
* Node.js - async I/O, but you have to deal with it manually because it doesn't have fibers like Ruby. Really there is no reason to use Node.js anymore unless programming in javascript is such an imperative that you want to deal with the asynchronous flow. It does perform better than Goliath, but if you really need that you may as well use Erlang or Haskell.
* Java - can Ruby actually scale better than Java now? Java will always have a *raw* speed advantage, but uses blocking IO. Java uses OS threads, so it can take on only as many simultaneous users with blocking database requests as the OS can schedule threads. Java's raw speed may makeup for the overhead of OS threas. But I suspect someone could create an IO heavy benchmark that Ruby can beat Java on in terms of requests per second. Perhaps not now, but instead when there is a mature multi-core Rubinius implementation.
* Erlang  - non-blocking I/O by default, runtime that scales actors to multi-core. Amazing distributed system capabilities (that your web application probably doesn't need).
* Haskell - non-blocking I/O by default, runtime that scales cheap threads to multi-core. It is a compiled language capable of very fast execution. I am a contributor to the [Yesod web framework](http://yesodweb.com), which is [much, much faster](http://www.yesodweb.com/blog/preliminary-warp-cross-language-benchmarks) than Goliath, and scales better than any web application server I know of.


## Conclusion

Please use asynchronous drivers for your database and any other IO! And try out an asynchronous framework and help make them better. We need to make non-blocking IO the default, at which point Ruby applications will have a much easier time scaling. I know this isn't a magic bullet- you are still going to have to scale your database and make sure you don't blow up memory usage in your Ruby code. But it will translate to better performance, reduced memory consumption, and simpler deployments.

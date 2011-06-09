---
title: High Performance Ruby Part 2: your default data store
description: MongoDB is a fast and flexible alternative to SQL
tags: ruby,mongodb
---

This is part 2 of a series. [Part 1](/posts/2011-06-03-high-performance-rb-part1) was on fibers, enumerators and scalbale XML parsing.

## MongoDB is a better default data store for most web applications.

For any given data storage and retrieval problem, there is likely a better optimized solution than a standard relational SQL database. But when you start a project off, it isn't always easy to predict the data storage issues- you still need a good default.

Most SQL databases trade off too much in terms of performance and features in order to have 2 things that are actually low on the need list of most web applications:

  * relational tables
  * transactions
  
If you need transactions, then by all means keep using a SQL database, although if you are really focused on that (OLTP) there are probably better alternatives out there.
  
Some aspects of relational tables don't scale well, and there are also better ways of modeling data.
  
### The need for speed

One of the standard answers to slow reads in SQL is replication. MongoDB comes with good replication built in. Another answer in SQL is to cache with memcache. In MongoDB, the indexes are in memory, so you can achieve good performance as long as you hit the indexes and have enough memory for them. To increase memory, you can put separate collections on separate machines without worry because there are no joins.
  
The other approach to faster reads is caching. You can start off your caching infrastructure with MongoDB itself because of its fast writes and reads and schemaless storage.

Generally when sites get very heavy traffic, they stop doing database joins and do them instead in the application. Perhaps your site will never reach that point, but if there are 2 equal alternatives, it only makes sense to use that one that scales.
  
For slow writes in SQL, there aren't many good answers. MongoDB was designed for modern usage patterns, not to fit the relational SQL constraints- writes are *much* faster. As the author of _High Performance MySQL_ [stated](http://blog.mongodb.org/post/5545198613/mongodb-live-at-craigslist):

    We can put data into MongoDB faster than we can get it out of MySQL during the migration.


### Faster use cases

On a previous project I needed to do lots of updates to some counters in MySQL. I ended up writing out the updates to a file and then doing a single update to the counter representing all the updates written to the file. Not a horrible solution, but it adds complexity and makes the data not real-time. In MongoDB you would just update the counters with an atomic increment or decrement operation and be done.

I have also dealt with trying to fit schemaless data into MySQL, and found it to be an absolute nightmare. Many users are doing this in MySQL with blobs of data serialized to JSON or binary. However, they give up the ability to query this data in the database. The solutions that allow you to query schemaless data all perform poorly in MySQL. Again, in MongoDB it is a non-issue.
  
I don't want to make MongoDB sound like a magic bullet. For the 2 use cases I just listed, it can be. However, for most read queries it is not likely to be noticeably faster than SQL, and many smaller sites don't care greatly about write speed. Again, I am just making the case for why it can be a better default.


### Richer data modeling

  Schemaless sounds scary, but it is a nice tool for richer data modeling. For example, you can store hashes (where the keys present are not known ahead of time).

  In MongoDB your alternative to database joins is not just application joins, but also embedding. Stick a table within another table, or stick an array of tables in another table. Not only do you feel little pain from not having joins, but the embedding concept lets you model your data in very meaningful ways that are very efficient for your application queries. Instead of a join table, you could add an array of ids to one of your documents (tables). And you can treat that array like a Set.


## Supplement vs. Default

NoSQL data stores are mostly used in very appropriate *supplementary* roles now. But the reason why most people will use MySQL as their primary data store on their next project is because they used it on their last. And that makes sense, because there is a big investment in using a new tool, and it may not be worth it. But its time to start learning MongoDB so you can know your options. Maybe you have libraries that work well with SQL, and there aren't easy alternatives for MongoDB, but at least be sure that is the case.

Lately I have been working with [YapTV](http://www.yap.tv). They have been running a hybrid MongoDB-MySQL architecture. Originally MongoDB was used for write intensive data and to cache responses. But YapTV keeps getting more comfortable with MongoDB, and MongoDB itself keeps maturing, YapTV is now moving as much of their data storage infrastructure from MySQL to MongoDB as possible.


## An ORM is essential

MongoDB is schema-less. The drivers do a good job of supporting schema-less data, which can be very important for certain problems. However, for most problems we can create a good schema for much of the data in advance, and it is essential for our ORM to enforce it to avoid type errors. As a Rails developer, I don't view this as a downside- I am already used to ActiveRecord enforcing the schema and business rules.

Mongoid is a fast maturing ORM for MongoDB. It is suprisingly easy to switch code from the new ActiveRecord Arel syntax to Mongoid- Mongoid uses some aspects of ActiveModel, and maintains a mostly compatible query api. There are certainly some rough edges still, but it is a mature enough solution today, and there is a growing community of users.

Sometimes you may need more performance than the ORM is capable of. In my tests I found using the raw ruby driver to be 10x faster than going through Mongoid. It also uses cursors by default to keep memory constant. But you have to be careful, particularly on inserts because you will bypass validations and can end up inserting the wrong types. I always start with Mongoid, and increase test coverage of the code in question before switching it to use the raw driver.


## Quick MongoDB tips

* Proper indexes make a big difference
* Think outside the SQL box when data modeling- you have a lot more freedom and a lot more options
  * modeling should match the queries
  * when performance is critical
    * use embeded objects a lot
    * Denormalize any data that is not updated by users


## Even more options

We have just scratched the surface of 2 data store options. Redis is very popular with Rubyists for fast, non-persistent data manipulation.

I am discussing MongoDB in part out of my ignorance of alternative data stores, but also out of ignorance of interfaces to other data stores. If you know of good interfaces to other persistent NoSQL data stores, please leave a comment.

## Keep it asynchronous!

Whatever data storage layer you use, try out an asynchronous/non-blocking driver. The next post will get into how you can really reap the rewards from non-blocking drivers.

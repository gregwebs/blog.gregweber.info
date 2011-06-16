---
title: High Performance Ruby Part 1: declarative & scalable XML parsing
description: Applying Fibers to make sax-machine scale
tags: ruby,xml
---

This is part 1 of a series. [Part 2](/posts/2011-06-08-high-performance-rb-part2) is on data storage.

# Scalability is king when it comes to performance

High Performance and Ruby traditionally haven't gone well together. And they still don't today. But we are getting to the point where Ruby programs can at least be operated in a manner that scales well. And if we are hooking into C libraries and accessing databases, we might not care so much about Ruby's raw performance anyways, as long as the memory doesn't blow up and we can do multiple things at once.


# Fibers- the key to scalable Ruby code

Scalability concerns can be different depending on the context. Often it means constant memory usage and/or the ability to perform non-blocking IO (take on more concurrent users in a web application). Both of these concerns can often be handled elegantly by the construct introduced in Ruby 1.9: fibers.

A Fiber is a light-weight thread of execution that can be paused or resumed by the programmer. We might be comfortable handing off all of the task of pausing and resuming to a better runtime, particularly in the case of non-blocking IO. But it turns out this is not that hard to manage, and it only has to be done once by the library writer- the implementation details become transparent to the library user.

By the end of this series you should be convinced of the importance of fibers. Unfortunately MRI 1.9 is still the king of fiber performance, but expect JRuby & Rubinius to catch-up.

# XML: Constant memory usage with enumerators and fibers

I have led a sheltered life: I had to do very little XML processing in Ruby until recently. Recently I had to deal with huge XML files that can't fit into memory, and I was appalled at the available XML tools. Lets start with a quick review of the main libraries. ReXML is a pure Ruby library, and thus horribly slow compared to the C alternatives: it isn't a realistic option. The other option is to use a library that wraps libxml (a fast C library), of which Nokogiri seems to be the most popular and best maintained. But we are still talking about raw performance. What is even more important to us for a large file is constant memory usage. To achieve this, we must use a SAX parser or a pull parser (very similar). However, once we commit to a SAX parser, all the nicities we expect from Ruby go away. We get 3 notification from the SAX parser: when an element starts, when it ends, and what inner content there is. This would be fine, except that we have to manually keep track of which element we are using:

~~~~~~~~~~~~~~~~~~~~~~~~ {.ruby}
  def start_element tag, attributes
    case tag
    when 'name'
      @attribute = 'name'
    when 'titles'
      @attribute = 'name'
    end
  end

  def end_element tag
    @model.save! if tag == 'name'
  end

  def characters text
    @model[@attribute] = text
  end
~~~~~~~~~~~~~~~~~~~~~~~~


This kind of coding just shouldn't be acceptable today. Other Rubyists have felt this pain and created tools to automatically handle the mapping of XML to Ruby objects. Once such library is sax-machine. Unfortunately, sax-machine, like all the other XML mappers I came across does not operate in constant memory. Even though the SAX parser does, a sax-machine program will buildup Ruby objects until it is done parsing the file, effectively negating the point of SAX parsing, and giving horrible performance for large files.

The solution to this is to process our objects one-by-one and discard the old ones, just as a SAX parser can process a file line-by-line and discard the old lines. We had an array of Ruby objects, but now we want a stream of them. In Ruby 1.8 we would have to use Ruby's yield and the Enumerable API, exposing an API for application code like this:

~~~~~~~~~~~~~~~~~~~~~~~~ {.ruby}
  parse do |object|
    object.save!
  end
~~~~~~~~~~~~~~~~~~~~~~~~

And internally the library code would look like this.

~~~~~~~~~~~~~~~~~~~~~~~~ {.ruby}
  def parse &block
    yield get_next_object
  end
~~~~~~~~~~~~~~~~~~~~~~~~

And this works fine for the most part. However, it does constrain our API, and we have to pass that block around in our library. To make a compatible API in the case of sax-machine, we need to return a lazy object that won't start parsing until the user requests the first object. The user's code now looks like this:

~~~~~~~~~~~~~~~~~~~~~~~~ {.ruby}
  parse.objects.each do |object|
    object.save!
  end
~~~~~~~~~~~~~~~~~~~~~~~~

Ruby 1.9's fibers pausing and resuming allows us to manage our library without requiring a block from the library user. And using Ruby 1.9's enumerators (which are implemented with fibers!) wraps this up into a nice enumerator stream. Here is our calling point in sax-machine. Note that we can maintain the same API, but in one case use a lazy option to signify needing a stream.


~~~~~~~~~~~~~~~~~~~~~~~~ {.ruby}
  def parse(thing, options = {}) 
    if options[:lazy]
      require 'fiber'
      @parser = Fiber.new do  
        Nokogiri::XML::SAX::Parser.new( SAXHandler.new(self) ).parse(thing)
      end 
    else
      Nokogiri::XML::SAX::Parser.new( SAXHandler.new(self) ).parse(thing)
    end 
    self
  end 
~~~~~~~~~~~~~~~~~~~~~~~~


Here is a simplified version of the old and new code for adding a parsed object:

~~~~~~~~~~~~~~~~~~~~~~~~ {.ruby}
  def add_object(obj)
    collection << obj
  end

  def add_object(obj)
    Fiber.yield obj
  end
~~~~~~~~~~~~~~~~~~~~~~~~

And here is their old and new retrieval:

~~~~~~~~~~~~~~~~~~~~~~~~ {.ruby}
  def objects
    collection
  end

  def objects
    Enumerator.new do |yielderr|
      while r = @parser.resume
        yielderr << r
      end
    end
  end
~~~~~~~~~~~~~~~~~~~~~~~~

The end result is constant memory usage with the nice enumerator streaming API. Lets see it in action:

~~~~~~~~~~~~~~~~~~~~~~~~ {.ruby}
  class AtomEntry
    include SAXMachine
    element :title
    # the :as argument makes this available through atom_entry.author instead of .name
    element :name, :as => :author
    element "feedburner:origLink", :as => :url
    element :summary
    element :content
    element :published
  end

  class Atom
    include SAXMachine
    element :title
    elements :entry, :lazy => true, :as => :entries, :class => AtomEntry
  end

  feed = Atom.parse(xml_file_handle, :lazy => true)
  feed.entries # => #<Enumerator: #<Enumerator::Generator:0x00000004c41ea0>:each> 
  feed.entries.each do |entry|
    # every time the block is called the next entry is parsed- no memory blow up! 
    # This is probably where you save the entry to a database
  end
~~~~~~~~~~~~~~~~~~~~~~~~

This code is now in the [main sax-machine repo](https://github.com/ezkl/sax-machine)

Now we have reached the holy grail of programming: declarative, scalable code. Instead of manually tracking state we just change our declarations. We have constant memory usage and good performance. Note that this is significantly slower than just using the raw SAX parser. In my rough tests I found the performance of this new sax-machine code to be halfway between the REXML SAX parser and the raw Nokogiri SAX parser. But there are always opportunities for optimizing the sax-machine library, including [modifying the nokogiri interface](http://www.pauldix.net/2009/01/making-a-ruby-c-library-even-faster.html).

Thanks to [YapTV](http://www.yap.tv) for sharing their work with the community.

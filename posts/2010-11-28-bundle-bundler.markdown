---
title: how to bundle bundler
description: how to lock down bundler to a specific version
tags: ruby
---

Bundler is now a critical piece of Ruby infrastructure. Combined with RVM it means we are guaranteed anyone can easily start working on a project (well, except for things like C dependencies). Bundler guarantees that everyone is working with the same gems, and a .rvrmc file can specify the exact ruby version. But wait, we haven't specified what version of bundle is being used- bundler cannot bundle bundler! To require a minimum version of bundler, you can add this to your Gemfile:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ruby}
# Enforce a bundler version - we always want to be on the latest version
# use lambda to avoid creating a top-level variable
lambda do
  min_bundler_version = '1.0.7'
  # There is a first pass and a second pass with different settings
  if Gem::Version.new(Bundler::VERSION) < Gem::Version.new(min_bundler_version)
    fail "Bundler version #{min_bundler_version} or greater required.  Please run 'gem update bundler'."
  end 
end.call
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

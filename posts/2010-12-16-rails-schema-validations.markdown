---
title: announcing rails-schema-validations
description: a ruby gem to automatically run validations based on your schema
tags: ruby rails
---

I was on a project to create an administrative backend to a site that already had a well constrained schema. I wanted to keep things DRY, so I created a gem to automatically generate rails validations from schema constraints.  This code has been in production for months now- it is solid and lets us worry a lot less when there is a schema change.

I was actually very suprised that such a tool didn't already exist. I did come across some existing code for Rails 1 that wasn't on github. And schema constraints are usually useful for performance reasons, so just declaring rails validations won't give you all the benefits of declaring schema constraints. Note that you will still want to use rails validations for things that cannot be practically encoded in a schema constraint.

Add `rails-schema-validations` to your gemfile and then place `validations_from_schema` in your model. There are more technical details in the [README](https://github.com/gregwebs/rails-schema-validations), including limitations, such as it has only been tested for MySQL and only works on certain column types. The code was suprisingly easy to write though, so fork away and add improvements for different column types. It is well tested, so run the test suite against your database of choice.

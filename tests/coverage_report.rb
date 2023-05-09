#!/usr/bin/env ruby

# frozen_string_literal: true

require 'simplecov'

SimpleCov.collate Dir['coverage/.resultset.json'] do
  # enable_coverage :branch
  formatter SimpleCov::Formatter::MultiFormatter.new([SimpleCov::Formatter::SimpleFormatter,
                                                      SimpleCov::Formatter::HTMLFormatter])
end

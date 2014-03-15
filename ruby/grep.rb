#!/usr/bin/env ruby

##
# Tiny grep inspired by grin (BIG python grep :D)

# Allow to use colored output functions
require "./init"

if ARGV.empty?
  puts "Usage: grep pattern [files ...]"
  exit 0
end

# Compile pattern
pattern = Regexp.new ARGV.shift

ARGV.each do |file|
  i = 0
  found = false

  if !File.exist? file
    # Ignore invalid filepath
    next
  end

  File.readlines(file).each do |line|
    i += 1
    if pattern =~ line
      if !found
        # First occurence for this file, print colored file path
        puts bold green "#{file}:"
        found = true
      end
      puts "  #{i} : #{line}"
    end
  end
end

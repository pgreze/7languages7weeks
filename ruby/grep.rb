#!/usr/bin/env ruby

require "./init"

if ARGV.empty?
  puts "Usage: grep pattern [files ...]"
  exit 0
end

pattern = Regexp.new ARGV.shift
ARGV.each do |file|
  i = 0
  found = false

  if !File.exist? file
    next
  end

  File.readlines(file).each do |line|
    i += 1
    if pattern =~ line
      if !found
        puts bold green "#{file}:"
        found = true
      end
      puts "  #{i} : #{line}"
    end
  end
end

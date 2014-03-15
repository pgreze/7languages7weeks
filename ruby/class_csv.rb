#!/usr/bin/env ruby

##
# Allow to use header names + common array methods

class CsvRow < Array

  attr_accessor :headers

  def initialize headers, contents
    @headers = headers
    super contents
  end

  def method_missing(meth, *args, &block)
    header = meth.to_s
    if @headers.include? header
      # Valid header, return element
      self[@headers.index header]
    else
      # Raise a NoMethodError "undefined method #{meth} for #{self}"
      super
    end
  end

end

module ActsAsCsv

  ##
  # Extend class (RubyCsv here) with ClassMethods module

  def self.included(base)
    base.extend ClassMethods
  end

  ##
  # Allow to use acts_as_csv method in ActsAsCsv's class

  module ClassMethods
    def acts_as_csv
      include InstanceMethods
    end
  end

  module InstanceMethods

    def read
      @csv_contents = []
      filename = self.class.to_s.downcase + '.txt'
      file = File.new(filename)

      # Read headers
      @headers = file.gets.chomp.split(', ')

      # Read contents
      file.each do |row|
        @csv_contents << CsvRow.new(@headers, row.chomp.split(', '))
      end
    end

    # Apply a procedure on each CsvRow
    def each(&block)
      csv_contents.each {|e| block.call e}
    end

    attr_accessor :headers, :csv_contents
    
    def initialize
      read
    end

  end

end

class RubyCsv
  include ActsAsCsv
  acts_as_csv
end

m = RubyCsv.new
puts ">> Headers"
puts m.headers.inspect
puts

puts ">> Content"
puts m.csv_contents.inspect
puts

puts ">> One columns uppercased"
m.each {|row| puts row.one.upcase}
puts

puts ">> Test invalid header behavior"
m.csv_contents[0].pedobear

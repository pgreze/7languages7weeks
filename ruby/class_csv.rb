#!/usr/bin/env ruby

class CsvRow < Array

  def initialize headers, content
    @headers = headers
    super content
  end

  def method_missing(meth, *args, &block)
    header = meth.to_s
    if @headers.include?(header)
      self[@headers.index header]
    else
      # Raise a NoMethodError "undefined method #{meth} for #{self}"
      super
    end
  end

end

module ActsAsCsv

  def self.included(base)
    base.extend ClassMethods
  end

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
      @headers = file.gets.chomp.split(', ')
      file.each do |row|
        @csv_contents << CsvRow.new(@headers, row.chomp.split(', '))
      end
    end

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

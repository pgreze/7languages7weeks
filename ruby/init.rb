#!/usr/bin/env ruby

def colorize(text, color_code)
  "\e[#{color_code}m#{text}\e[0m"
end
def underline(text); colorize(text, 4); end
def bold(text); colorize(text, 7); end
def red(text); colorize(text, 31); end
def green(text); colorize(text, 32); end
def blue(text); colorize(text, 34); end
def violet(text); colorize(text, 35); end

if __FILE__ == $0
  # Print X times a sentence
  times = 5
  puts underline blue "Print a sentence #{times} times"
  (1..times).each { |i| puts "This is sentence number #{i}" }
  #puts Array.new(times) { |i| "This is sentence number #{i+1}" }.join("\n")

  search = /ruby/i
  text = "Hello Ruby !!"
  puts
  puts blue "#{search} regular expression with #{text} => #{search =~ text}"

  # Print the contents of an array of sixteen numbers, four numbers at a time
  (1..16).to_a.each_slice(4) { |i| puts i.join(",") }
end
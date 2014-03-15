#!/usr/bin/env ruby

max_rand = 100

random_number = rand max_rand

puts "Hello ! Guess the number between 0 and #{max_rand}"

input = -1
while (input = gets().to_i) != random_number
	if input < random_number
		puts "Too short"
	else
		puts "Too big"
	end
end

puts "Well done! o//"
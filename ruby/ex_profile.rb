require 'profile'

# count = 0
# File.foreach("/usr/share/dict/words") do |word|
#   word.chomp!
#   count += 1 if word.length == 12
# end

# puts "#{count} twelve-char words"

words = File.read("/usr/share/dict/words")
count = words.scan(/^............\n/).size
puts "#{count} twelve-char words"

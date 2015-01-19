def update_feed
  puts "Updating..."
  sleep 3
  puts "Feed UPDATED"
end
def convert
  puts "Changing rate"
end

rate_mutex = Mutex.new
Thread.new do
  loop do
    sleep 5
    rate_mutex.synchronize do
      update_feed
    end
  end
end

loop do
  print "Enter currency code and amount: "
  line = gets
  if rate_mutex.try_lock
    begin
      convert
    ensure
      rate_mutex.unlock
    end
  else
    puts "Sorry, rates being updated. Try again in 5 seconds"
  end
end

    


module Logger
  def log(msg)
    STDERR.puts Time.now.strftime("%H:%M:%S: ") + "#{self} (#{msg})"
  end
end

class Song
  # include adds Logger instance methods as Song's instance methods
  include Logger
end











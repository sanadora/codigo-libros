require 'optparse'

OptionParser.new do |opts|
  opts.banner = "Basic test..."
  opts.on("-s","--shout phrase","Phrase to be shouted.") do |phrase|
    puts phrase.upcase
  end

  opts.on("-h","--help", "Show this message") do
    puts opts
    exit
  end

  begin
    ARGV = ["-h"] if ARGV.empty?
    opts.parse!(ARGV)
  rescue OptionParser::ParseError => e
    STDERR.puts e.message, "\n", opts
    exit(-1)
  end
end
      



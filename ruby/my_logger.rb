class MyLogger
  
  def debug( message )
    @log ||= [] # @log = @log || []
    @log << "DEBUG: #{message}"
  end

  def error( message )
    @log ||= []
    @log << "ERROR: #{message}"
  end

  def show_logged
    @log.each {|log| puts log}
  end
end    

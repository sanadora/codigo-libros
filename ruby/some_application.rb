class SomeApplication

  def initialize( logger )
    @logger = logger
  end

  def do_something
    with_logging('load') { @doc = Document.load('resume.txt')}
    # Do something with the document...
    with_logging('save') { @doc.save }
  end

  def do_something_silly
    with_logging('Compute miles in a light year') do
      186000 * 60 * 60 * 24 * 365
    end
  end

  def with_logging(description)
    begin
      @logger.debug( "Starting #{description}")
      yield
      @logger.debug( "Completed #{description}")
    rescue
      @logger.error( "#{description} failed!")
      raise
    end
  end
end

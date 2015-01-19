# Class Representing A Text document, for learning purposes. (Eloquent
# Ruby book)
class Document
  attr_reader :title, :author, :content

  # defining class methods (singletons on Document object)
  class << self
    def find_by_name(name)
      #find document by name
      puts "Searching (by name)..."
    end

    def find_by_author(author)
      #fin document by author
      puts "Searching (by author)..."
    end
  end

  # another class method
  def self.create_test_document(length)
    Document.new('test','test','test'*length)
  end

  def initialize(title, author, content = '')
    @title, @author, @content = title, author, content
    yield( self ) if block_given?
  end

  def words
    @content.split
  end

  def word_count
    @content.split.size
  end

  def +(other)
    Document.new(title, author, "#{content} #{other.content}")
  end

  def !
    Document.new(title, author, "It's not true: #{content}")
  end

  def +@
    Document.new(title, author, "I'm sure that #{content}")
  end

  def -@
    Document.new(title, author, "I doubt that #{content}")
  end

  def [](index)
    words[index]
  end

  def size
    words.size
  end

  def each
    words.each {|word| yield word}
  end

  def each_character
    index = 0
    while index < @content.size
      yield @content[index]
      index += 1
    end
  end
end

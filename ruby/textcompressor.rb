# Example: "This specification is the specification for a
# specification"
# ["This", "specification", "is", "the", "for", "a"]
# [0, 1, 2, 3, 1, 4, 5, 1]
class TextCompressor
  attr_reader :unique, :index

  def initialize(text)
    @unique = []
    @index = []
    add_text(text)
  end

  def add_text(text)
    words = text.split
    words.each { |word| add_word(word) }
  end

  def add_word(word)
    i = unique_index_of(word) || add_unique_word(word)
    @index << i
  end

  def unique_index_of(word)
    @unique.index(word)
  end

  def add_unique_word(word)
    @unique << word
    @unique.size - 1
  end
end

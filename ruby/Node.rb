class Node
  include Enumerable
  include Comparable
  
  attr_accessor :data, :left, :right

  def initialize(data)
    @data = data
  end

  def each(&block)
    left.each(&block) if left
    block.call(self)
    right.each(&block) if right
  end

  def <=>(other)
    data <=> other.data
  end
end

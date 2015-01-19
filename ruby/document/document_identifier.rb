class DocumentIdentifier
  attr_reader :folder, :name

  def initialize(folder, name)
    @folder, @name = folder, name
  end

  def ==(other)
    return true if other.equal?(self)
    return false unless other.instance_of?(self.class)
    @folder == other.folder && @name == other.name
  end

  def hash
    folder.hash ^ name.hash
  end

  def eql?(other)
    return false unless other.instance_of?(self.class)
    folder == other.folder && name == other.name
  end
end

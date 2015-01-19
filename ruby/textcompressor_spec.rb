require_relative 'textcompressor'

describe TextCompressor do

  it "should be able to add some text" do
    c = TextCompressor.new( '' )
    c.add_text('first second')
    expect( c.unique ).to eq(['first', 'second'])
    expect( c.index ).to eq([0, 1])
  end

  it "should be able to add a word" do
    c = TextCompressor.new( '' )
    c.add_word( 'first' )
    expect( c.unique ).to eq( ['first'] )
    expect( c.index ).to eq( [ 0 ] )
  end

  it "should be able to find the index of a word" do
    c = TextCompressor.new( 'Hello World' )
    expect( c.unique_index_of( 'Hello' )).to eq 0
    expect( c.unique_index_of( 'World' )).to eq 1
  end
end
  
      

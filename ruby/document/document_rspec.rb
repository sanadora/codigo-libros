require_relative 'document'

describe Document do
  before :each do
    @text = "A bunch of words"
    @doc = Document.new('test', 'nobody', @text)
  end
  
  it 'should hold on to the contents' do
    expect(@doc.content).to eq @text
  end

  it 'should know which words it has' do
    expect(@doc.words).to include('A')
    expect(@doc.words).to include('bunch')
    expect(@doc.words).to include('of')
    expect(@doc.words).to include('words')
  end

  it 'should know how many words it contains' do
    expect(@doc.word_count).to eq 4
  end

  it 'should use the \'+\' operator correctly (sum contents)' do
    doc2 = Document.new('tit', 'aut', 'extra')
    @doc = @doc + doc2
    expect(@doc.content).to eq 'A bunch of words extra'
  end
end
    

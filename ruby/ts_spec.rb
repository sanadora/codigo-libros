require 'tennis_scorer'

describe TennisScorer, "basic scoring" do
  before(:each) do
    @ts = TennisScorer.new
  end
  
  it "should start with a score of 0-0" do
    #ts.score.should == "0-0"
    expect(@ts.score).to eq("0-0")
  end
  
  it "should be 15-0 if the server wins a point" do
    @ts.give_point_to(:server)
    expect(@ts.score).to eq("15-0")
  end
  
  it "should be 0-15 if the receiver wins a point" do
    @ts.give_point_to(:receiver)
    expect(@ts.score).to eq("0-15")
  end
  
  it "should be 15-15 after they both win a point" do
    @ts.give_point_to(:server)
    @ts.give_point_to(:receiver)
    expect(@ts.score).to eq("15-15")
  end
end

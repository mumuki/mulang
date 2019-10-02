class Mulang::Expectation::Custom
  attr_accessor :name

  def initialize(name)
    @name = name
  end

  def translate(*)
    name
  end

  def to_h
    {binding: '<<custom>>', inspection: name}
  end

  def custom?
    true
  end

  def standard?
    false
  end

  def self.parse(expectation)
    new expectation[:inspection]
  end

  def self.valid?(_)
    true
  end
end

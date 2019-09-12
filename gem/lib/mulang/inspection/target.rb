class Mulang::Inspection::Target
  attr_accessor :type, :value

  def initialize(type, value=nil)
    @type = type
    @value = value
  end

  def self.parse(target_s)
    if target_s.blank?
      nil
    elsif target_s == '*'
      anyone
    elsif target_s.start_with? '^'
      new :except, target_tail(target_s)
    elsif target_s.start_with? '~'
      new :like, target_tail(target_s)
    elsif target_s.start_with? '='
      named target_tail(target_s)
    else
      unknown target_s
    end
  end

  def to_s
    case type
    when  :anyone
      '*'
    when :except
      "^#{value}"
    when :like
      "~#{value}"
    when :named
      "=#{value}"
    else
      value
    end
  end

  def i18n_suffix
    case type
    when :anyone
      nil
    when :except
      "_except"
    when :like
      "_like"
    else
      "_named"
    end
  end

  def self.target_tail(target_s)
    target_s[1..-1]
  end

  def self.unknown(value)
    new(:unknown, value)
  end

  def self.named(value)
    new(:named, value)
  end

  def self.anyone
    new(:anyone)
  end
end

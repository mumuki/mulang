require 'mumukit/core'
require 'open3'

require_relative './mulang/version'

I18n.load_translations_path File.join(__dir__, 'locales', '*.yml')

module Mulang
  def self.bin_path
    File.join(__dir__, '..', 'bin', 'mulang')
  end
  def self.analyse(analysis, **options)
    serialization = options[:serialization]
    many = analysis.is_a?(Array)
    mode = many || serialization ? encode_serialization(serialization) : '-s'

    Open3.popen2(bin_path, mode) do |input, output, _thread|
      input.puts (!many && serialization ? [analysis] : analysis).to_json
      input.close
      result = JSON.parse output.read
      !many && serialization ? result[0] : result
    end
  end

  private

  def self.encode_serialization(option)
    case option
    when nil then '-S'
    when :bracket then '-B'
    when :brace then '-C'
    else raise "Unsupported serialization #{option}"
    end
  end
end

require_relative './mulang/tokens'
require_relative './mulang/inspection'
require_relative './mulang/expectation'
require_relative './mulang/code'
require_relative './mulang/language'

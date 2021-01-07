require 'mumukit/core'
require 'open3'

require_relative './mulang/version'

I18n.load_translations_path File.join(__dir__, 'locales', '*.yml')

module Mulang
  def self.bin_path
    File.join(__dir__, '..', 'bin', 'mulang')
  end
  def self.analyse(analysis)
    mode = analysis.is_a?(Array) ? '-S' : '-s'

    Open3.popen2(bin_path, mode) do |input, output, _thread|
      input.puts analysis.to_json
      input.close
      JSON.parse output.read
    end
  end
end

require_relative './mulang/tokens'
require_relative './mulang/inspection'
require_relative './mulang/expectation'
require_relative './mulang/code'
require_relative './mulang/language'

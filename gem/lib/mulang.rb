require 'active_support/all'
require 'open3'

require_relative './mulang/version'

module Mulang
  def self.bin_path
    File.join(__dir__, '..', 'bin', 'mulang')
  end
  def self.analyse(analysis)
    Open3.popen2(bin_path, '-s') do |input, output, _thread|
      input.puts analysis.to_json
      input.close
      JSON.parse output.read
    end
  end
end

require_relative './mulang/code'
require_relative './mulang/language'

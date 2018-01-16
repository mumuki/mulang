require 'active_support/all'

require_relative './mulang/version'

module Mulang
  def self.bin_path
    File.join(__dir__, '..', 'bin', 'mulang')
  end
  def self.analyse(analysis)
    IO.popen([bin_path, analysis.to_json]) { |f| JSON.parse f.read }
  end
end

require_relative './mulang/code'
require_relative './mulang/language'

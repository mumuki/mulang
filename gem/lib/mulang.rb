require 'mumukit/core'
require 'open3'

require_relative './mulang/version'

I18n.load_translations_path File.join(__dir__, 'locales', '*.yml')

module Mulang
  def self.bin_path
    File.join(__dir__, '..', 'bin', 'mulang')
  end
  def self.analyse(analysis, **options)
    arg, mode = Mulang::RunMode.for analysis, options
    Open3.popen2(bin_path, arg) do |input, output, _thread|
      input.puts mode.input(analysis).to_json
      input.close
      result = JSON.parse output.read
      mode.output result
    end
  end

  private

  module RunMode
    def self.for(analysis, options)
      serialization = options[:serialization]
      many = analysis.is_a?(Array)
      if many
        [encode_serialization(serialization), Mulang::RunMode::Natural]
      elsif serialization
        [encode_serialization(serialization), Mulang::RunMode::ForcedMany]
      else
        ["-s", Mulang::RunMode::Natural]
      end
    end

    def self.encode_serialization(option)
      case option
      when nil then '-S'
      when :bracket then '-B'
      when :brace then '-C'
      else raise "Unsupported serialization #{option}"
      end
    end

    module Natural
      def self.input(analysis)
        analysis
      end

      def self.output(result)
        result
      end
    end

    module ForcedMany
      def self.input(analysis)
        [analysis]
      end

      def self.output(result)
        result[0]
      end
    end
  end
end

require_relative './mulang/tokens'
require_relative './mulang/inspection'
require_relative './mulang/expectation'
require_relative './mulang/code'
require_relative './mulang/language'

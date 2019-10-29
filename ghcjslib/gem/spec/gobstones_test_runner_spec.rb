require "spec_helper"

describe MulangJs do
  it { expect(File.exist? MulangJs.assets_path_for('javascripts/mulang.js')).to be true }
end

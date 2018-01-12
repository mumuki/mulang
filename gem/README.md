[![Build Status](https://travis-ci.org/mumuki/mulang.rb.svg?branch=master)](https://travis-ci.org/mumuki/mulang.rb)
[![Code Climate](https://codeclimate.com/github/mumuki/mulang.rb/badges/gpa.svg)](https://codeclimate.com/github/mumuki/mulang.rb)
[![Test Coverage](https://codeclimate.com/github/mumuki/mulang.rb/badges/coverage.svg)](https://codeclimate.com/github/mumuki/mulang.rb)
[![Issue Count](https://codeclimate.com/github/mumuki/mulang.rb/badges/issue_count.svg)](https://codeclimate.com/github/mumuki/mulang.rb)


# Mulang

> Ruby Gem Wrapper for the [mulang](github.com/mumuki/mulang) code anayzer.

## Installation

Add this line to your application's Gemfile:

```ruby
gem 'mulang'
```

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install mulang

## Usage

### Basic analysis execution

```ruby
require 'mulang'

Mulang.analyse sample: {
                  tag: 'CodeSample',
                  language: 'JavaScript',
                  content: 'var x = 1'
               },
               spec: {
                expectations: [],
                smellsSet: {
                  tag: 'NoSmells'
                }
              }
# => {"tag"=>"AnalysisCompleted", "intermediateLanguage"=>nil, "signatures"=>[], "smells"=>[], "expectationResults"=>[]}
```

### Code manipulation

```ruby
code = Mulang::Code.new(Mulang::Language::Native.new('JavaScript'),  'var x = 1')

# generate ast
code.ast
# => {"tag"=>"Variable", "contents"=>["x", {"tag"=>"MuNumber", "contents"=>1}]}

code.analysis expectations: [], smellsSet: { tag: 'NoSmells' }
# => {:sample=>{:tag=>"CodeSample", :language=>"JavaScript", :content=>"var x = 1"}, :spec=>{:expectations=>[], :smellsSet=>{:tag=>"NoSmells"}}}

code.analyse expectations: [], smellsSet: { tag: 'NoSmells' }
# => {"tag"=>"AnalysisCompleted", "intermediateLanguage"=>nil, "signatures"=>[], "smells"=>[], "expectationResults"=>[]}
```

## Development

After checking out the repo, run `bin/setup` to install dependencies. Then, run `rake spec` to run the tests. You can also run `bin/console` for an interactive prompt that will allow you to experiment.

### Installing Locally

To install this gem onto your local machine, run `bundle exec rake install`.

### Updating Mulang version

Edit `version.rb` with a new Mulang version, and then run `bin/setup` again.

### Releasing a new version

To release a new version:

1. update `VERSION` in `version.rb`
2. ensure the `MULANG_VERSION` in `version.rb` points to an existing mulang tag
3. commit the version change and tag the repository as `v$VERSION`
4. push the repository and travis will do the rest :smile: - including pushing the gem file to [rubygems.org](https://rubygems.org).

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/[USERNAME]/mulang. This project is intended to be a safe, welcoming space for collaboration, and contributors are expected to adhere to the [Contributor Covenant](http://contributor-covenant.org) code of conduct.

## License

The gem is available as open source under the terms of the [MIT License](http://opensource.org/licenses/MIT).

## Code of Conduct

Everyone interacting in the Mulang project’s codebases, issue trackers, chat rooms and mailing lists is expected to follow the [code of conduct](https://github.com/[USERNAME]/mulang/blob/master/CODE_OF_CONDUCT.md).

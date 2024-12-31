require_relative "lib/ruty/version"

Gem::Specification.new do |spec|
  spec.name          = "ruty"
  spec.version       = Ruty::VERSION
  spec.summary       = "The typed Ruby"
  spec.homepage      = "https://github.com/qnighy/ruty"
  spec.license       = "MIT"

  spec.author        = "Masaki Hara"
  spec.email         = "ackie.h.gmai@gmail.com"

  spec.files         = Dir["*.{md,txt}", "{ext,lib}/**/*", "Cargo.*", ".yardopts"]
  spec.require_path  = "lib"
  spec.extensions    = ["ext/ruty/extconf.rb"]

  spec.required_ruby_version = ">= 3.2"

  spec.add_dependency "rb_sys"
end

# frozen_string_litera: true

require "ruty/version"
begin
  require "ruty/#{RUBY_VERSION.to_f}/ruty.so"
rescue LoadError
  require "ruty/ruty.so"
end

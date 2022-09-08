$LOAD_PATH.unshift(File.join(__FILE__, "../lib"))
VERSION = File.read(File.expand_path("../VERSION", __FILE__))

Gem::Specification.new do |s|
  s.name        = "tee-io"
  s.version     = VERSION
  s.summary     = "tee.io client SDK"
  s.license     = "MIT"
  s.authors     = "Pat Brisbin"
  s.email       = "pbrisbin@gmail.com"
  s.homepage    = "https://tee-io.onrender.com"
  s.description = "Client SDK for interfacing with tee.io"

  s.files         = Dir["lib/**/*.rb"]
  s.require_paths = ["lib"]

  s.add_dependency "curb", "~>0.8.8"
  s.add_dependency "posix-spawn", "~>0.3.11"

  s.add_development_dependency "rake"
  s.add_development_dependency "rspec"
  s.add_development_dependency "webmock"
end

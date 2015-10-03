require "tee_io"

TeeIO.run("../../bin/example", &method(:puts))

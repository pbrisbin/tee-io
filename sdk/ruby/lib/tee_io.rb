require "curb"
require "json"
require "posix/spawn"
require "timeout"

require "tee_io/api"
require "tee_io/process"
require "tee_io/token_response"

module TeeIO
  DEFAULT_URL = "https://tee-io.onrender.com"
  DEFAULT_TIMEOUT = 5 * 60

  def self.run(*command, description: nil, base_url: DEFAULT_URL, timeout: DEFAULT_TIMEOUT)
    api = API.new(base_url)
    resp = api.create_command(description || "#{command.join(" ")}")
    token = TokenResponse.new(resp).token

    yield "#{base_url}/commands/#{token}"

    process = Process.new(command, timeout)
    process.run do |_stream, io|
      io.each_line do |line|
        api.create_output(token, line)
      end
    end
  end
end

require "curb"

module TeeIO
  class API
    def initialize(base_url)
      @base_url = base_url
    end

    def create_command(description)
      request(:post, "/commands", description: description)
    end

    def create_output(token, content)
      request(:post, "/commands/#{token}/output", content: content)
    end

    private

    attr_reader :base_url

    def request(method, path, body)
      Curl.send(method, "#{base_url}#{path}", body.to_json) do |curl|
        curl.headers["Accept"] = "application/json"
        curl.headers["Content-Type"] = "application/json"
      end
    end
  end
end

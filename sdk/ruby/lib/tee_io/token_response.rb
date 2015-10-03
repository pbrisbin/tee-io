module TeeIO
  class TokenResponse
    InvalidResponseError = Class.new(StandardError)

    def initialize(response)
      code = response.response_code

      unless [200, 201].include?(code)
        raise_invalid!("#{code} #{response.body_str}")
      end

      @response = response
    end

    def token
      json_response["token"] or raise_invalid!("no token present")
    end

    private

    attr_reader :response

    def json_response
      JSON.parse(response.body_str)
    rescue JSON::ParserError => ex
      raise_invalid!("JSON parse error: #{ex.message}")
    end

    def raise_invalid!(message)
      raise InvalidResponseError,
        "Unable to start command #{message}"
    end
  end
end

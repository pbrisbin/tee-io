require "spec_helper"

module TeeIO
  describe TokenResponse do
    describe "#token" do
      it "returns the token" do
        response = http_success('{"token":"abc123"}')

        token = TokenResponse.new(response).token

        expect(token).to eq "abc123"
      end

      it "raises on unexpected JSON (no token)" do
        response = http_success('{"not_token":true}')

        expect_invalid_response(response, /no token/)
      end

      it "raises on unparsable JSON" do
        response = http_success("{invalid json")

        expect_invalid_response(response, /unexpected token/)
      end

      it "raises on unsuccessful HTTP responses" do
        response = double(response_code: 400, body_str: "bad method")

        expect_invalid_response(response, /400 bad method/)
      end
    end

    def http_success(body)
      double(response_code: 200, body_str: body)
    end

    def expect_invalid_response(response, message)
      expect { TokenResponse.new(response).token }.to raise_error(
        TokenResponse::InvalidResponseError, message
      )
    end
  end
end

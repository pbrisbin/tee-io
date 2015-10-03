require "spec_helper"

describe TeeIO do
  describe ".run" do
    let(:token) { "abc123" }
    let(:base_url) { "http://localhost:3000" }

    it "runs the command, streams to tee-io, and yields the URL" do
      requests = [
        r(:post, "/commands", "{\"description\":\"test command\"}", "{\"token\":\"#{token}\"}"),
        r(:post, "/commands/#{token}/output", "{\"content\":\"foo\\n\"}"),
        r(:post, "/commands/#{token}/output", "{\"content\":\"bar\\n\"}"),
        r(:post, "/commands/#{token}/output", "{\"content\":\"baz\\n\"}"),
      ]

      args = {
      }

      status = TeeIO.run(
        "sh", "-c", "echo foo; echo bar; echo baz >&2; false",
        description: "test command",
        base_url: base_url,
      ) do |url|
        expect(url).to eq "#{base_url}/commands/#{token}"
      end

      expect(status).not_to be_success
      requests.each { |req| expect(req).to have_been_made }
    end

    def r(method, path, request, response = "{}")
      stub_request(method, "#{base_url}#{path}").
        with(body: request).to_return(status: 200, body: response)
    end
  end
end

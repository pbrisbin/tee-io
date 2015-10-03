require "spec_helper"

module TeeIO
  describe API do
    let(:api) { API.new(base_url) }
    let(:base_url) { "http://localhost:3000" }

    describe "#create_command" do
      it "POSTs to /commands with the given description" do
        req = stub_request(:post, "#{base_url}/commands").
          with(body: "{\"description\":\"description\"}")

        api.create_command("description")

        expect(req).to have_been_requested
      end
    end

    describe "#create_output" do
      it "POSTs to /commands/:token/output with the given content" do
        req = stub_request(:post, "#{base_url}/commands/token/output").
          with(body: "{\"content\":\"content\"}")

        api.create_output("token", "content")

        expect(req).to have_been_requested
      end
    end
  end
end

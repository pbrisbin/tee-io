require "spec_helper"

module TeeIO
  describe Process do
    it "runs a process and streams output" do
      stdout = []
      stderr = []
      process = Process.new(
        ["sh", "-c", "echo stdout; echo stderr >&2"], 10
      )

      status = process.run do |stream, io|
        case stream
        when :stdout then stdout += io.each_line.to_a
        when :stderr then stderr += io.each_line.to_a
        end
      end

      expect(status).to be_success
      expect(stdout).to eq ["stdout\n"]
      expect(stderr).to eq ["stderr\n"]
    end

    it "wraps the process in a timeout" do
      process = Process.new(["sleep", "10"], 0.1)

      expect { process.run }.to raise_error(Timeout::Error)
    end
  end
end

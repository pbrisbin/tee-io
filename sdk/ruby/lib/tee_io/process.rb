module TeeIO
  class Process
    def initialize(command, timeout)
      @command = command
      @timeout = timeout
    end

    def run(&block)
      pid, stdin, out, err = POSIX::Spawn.popen4(*command)

      stdin.close
      stdout_thread = Thread.new { block.call(:stdout, out) }
      stderr_thread = Thread.new { block.call(:stderr, err) }

      status = Timeout.timeout(timeout) do
        ::Process.waitpid2(pid)[1]
      end

      stdout_thread.join
      stderr_thread.join

      status
    rescue Exception => ex
      if pid
        begin
          ::Process.kill("KILL", pid)
          ::Process.waitpid2(pid)
        rescue Errno::ESRCH
        end
      end

      stdout_thread.kill if stdout_thread
      stderr_thread.kill if stderr_thread

      raise
    end

    private

    attr_reader :command, :arguments, :timeout
  end
end

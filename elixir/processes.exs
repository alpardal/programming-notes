defmodule Processes do
  def spawn_new do
    IO.puts "Hi"
    _pid = spawn(fn -> IO.puts "Hi from child process" end)
  end

  def async_command do
    pid = spawn(
      fn ->
        receive do
          :hi -> IO.puts("Received a 'hi'\n")
        end
      end
    )
    send(pid, :hi)
  end

  def sync_command do
    pid = spawn(
      fn ->
        receive do
          {:hi, pid} ->
            IO.puts "<child> Received a 'hi', sending reply:"
            send(pid, :got_it)
        end
      end
    )
    send(pid, {:hi, self()})
    receive do
      :got_it -> IO.puts "<parent> Got the reply!"
    end
  end

  def timeouts do
    receive do
      :something -> IO.puts "I received something!"
    after
      0 -> IO.puts "I didn't receive anything after no time at all"
    end
  end

  def links do
    Process.flag(:trap_exit, true)
    pid = spawn_link(fn -> raise "oops" end)

    receive do
      {:EXIT, ^pid, reason} -> IO.puts "child crashed: #{inspect reason}"
      rest -> IO.inspect rest
    end
  end

  def monitors do
    pid = spawn(fn -> raise "an error" end)
    ref = Process.monitor(pid)

    receive do
      {:DOWN, ^ref, _, _, reason} ->
        IO.puts "died for: #{inspect reason}"
    end
  end

  def run_all do
    m = __MODULE__
    run(&m.spawn_new/0)
    run(&m.async_command/0)
    run(&m.sync_command/0)
    run(&m.timeouts/0)
    run(&m.links/0)
    run(&m.monitors/0)
  end

  defp run(fun) do
    name = Keyword.get(Function.info(fun), :name) |> to_string
    IO.puts "- #{name}:"
    fun.()
    IO.puts "----------------------------"
  end
end

Processes.run_all

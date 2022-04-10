defmodule KV do
  use GenServer

  def start do
    {:ok, pid} = GenServer.start(__MODULE__, nil)
    pid
  end

  def put(kv, key, value) do
    GenServer.cast(kv, {:put, key, value})
  end

  def get(kv, key) do
    GenServer.call(kv, {:get, key})
  end

  def stop(kv) do
    GenServer.call(kv, :stop)
  end

  @impl true     # or `@impl GenServer`
  def init(_) do
    {:ok, %{}}
  end

  @impl true
  def handle_cast({:put, key, value}, state) do
    {:noreply, Map.put(state, key, value)}
  end

  @impl true
  def handle_call({:get, key}, _, state) do
    {:reply, Map.get(state, key), state}
  end

  @impl true
  def handle_call(:stop, _, state) do
    {:stop, :normal, "\tyay, no more work", state}
  end

  @impl true
  def handle_info({:test, msg}, state) do
    IO.puts "\treceived a test message: #{inspect msg}"
    {:noreply, state}
  end
end

kv = KV.start
IO.puts "setting key: #{inspect KV.put(kv, :name, "Bob")}"
IO.puts "getting value: #{inspect KV.get(kv, :name)}"
IO.puts "inspecting server state: #{inspect :sys.get_state(kv)}"
IO.puts "generic message:"
send(kv, {:test, "bla"})
Process.sleep 10
IO.puts "gonna stop the server, w/ 'normal' termination:"
IO.puts KV.stop(kv)

defmodule KV.Registry do
  use GenServer

  ## Client API

  @doc """
  Starts the registry.
  """
  def start_link(table, event_manager, buckets, opts \\ []) do
    # 1. Pass the buckets supervisor as argument
    GenServer.start_link(__MODULE__, {table, event_manager, buckets}, opts)
  end

  def start_link(event_manager, buckets, opts \\ []) do
    # 1. Pass the buckets supervisor as argument
    GenServer.start_link(__MODULE__, {event_manager, buckets}, opts)
  end

  @doc """
  Looks up the bucket pid for `name` stored in `server`.

  Returns `{:ok, pid}` if the bucket exists, `:error` otherwise.
  """
  def lookup(server, name) do
    GenServer.call(server, {:lookup, name})
  end

  @doc """
  Looks up the bucket pid for `name` stored in `table`.

  Returns `{:ok, pid}` if a bucket exists, `:error` otherwise.
  """
  def lookup(table, name) do
    # 2. lookup now expects a table and looks directly into ETS.
    #    No request is sent to the server.
    case :ets.lookup(table, name) do
      [{^name, bucket}] -> {:ok, bucket}
      [] -> :error
    end
  end

  @doc """
  Ensures there is a bucket associated to the given `name` in `server`.
  """
  def create(server, name) do
    GenServer.call(server, {:create, name})
  end

  @doc """
  Stops the registry.
  """
  def stop(server) do
    GenServer.call(server, :stop)
  end

  ##  Server Callbacks
@doc """


  def init(events, buckets) do
    names = Map.new()
    refs  = Map.new()
    # 2. Store the buckets supervisor in the state
  {:ok, %{names: names, refs: refs, events: events, buckets: buckets}}
  end
"""

  def init({table, events, buckets}) do
    # 3. We have replaced the names HashDict by the ETS table
    ets  = :ets.new(table, [:named_table, read_concurrency: true])
    refs = Map.new
    {:ok, %{names: ets, refs: refs, events: events, buckets: buckets}}
  end

  def handle_call(:stop, _from, state) do
    {:stop, :normal, :ok, state}
  end

  @doc """
  def handle_call({:lookup, name}, _from, state) do
    {:reply, Map.fetch(state.names, name), state}
  end
"""
@doc """
  def handle_cast({:create, name}, {names, refs}) do
    if Map.has_key?(names, name) do
      {:noreply, {names, refs}}
    else
      {:ok, bucket} = KV.Bucket.start_link()
      ref = Process.monitor(pid)
      refs = Map.put(refs, ref, name)
      names = Map.put(names, name, pid)
      {:noreply, {names, refs}}
    end
  end
"""

def handle_cast({:create, name}, state) do
  # 5. Read and write to the ETS table instead of the HashDict
  case lookup(state.names, name) do
    {:ok, _pid} ->
      {:noreply, state}
    :error ->
      {:ok, pid} = KV.Bucket.Supervisor.start_bucket(state.buckets)
      ref = Process.monitor(pid)
      refs = Map.put(state.refs, ref, name)
      :ets.insert(state.names, {name, pid})
      GenEvent.sync_notify(state.events, {:create, name, pid})
      {:noreply, %{state | refs: refs}}
  end
end

def handle_cast({:create, name}, state) do
  if Map.get(state.names, name) do
    {:noreply, state}
  else
    # 3. Use the buckets supervisor instead of starting buckets directly
    {:ok, pid} = KV.Bucket.Supervisor.start_bucket(state.buckets)
    ref = Process.monitor(pid)
    refs = Map.put(state.refs, ref, name)
    names = Map.put(state.names, name, pid)
    # 3. Push a notification to the event manager on create
    GenEvent.sync_notify(state.events, {:create, name, pid})
    {:noreply, %{state | names: names, refs: refs}}
  end
end

  def handle_info({:DOWN, ref, :process, pid, _reason}, state) do
    {name, refs} = Map.pop(state.refs, ref)
    :ets.delete(state.names, name)
    #names = Map.delete(state.names, name)
    GenEvent.sync_notify(state.events, {:exit, name, pid})
    #{:noreply, %{state | names: names, refs: refs}}
    {:noreply, %{state | refs: refs}}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  def handle_call({:create, name}, _from, state) do
    case lookup(state.names, name) do
      {:ok, pid} ->
        {:reply, pid, state} # Reply with pid
      :error ->
        {:ok, pid} = KV.Bucket.Supervisor.start_bucket(state.buckets)
        ref = Process.monitor(pid)
        refs = Map.put(state.refs, ref, name)
        :ets.insert(state.names, {name, pid})
        GenEvent.sync_notify(state.events, {:create, name, pid})
        {:reply, pid, %{state | refs: refs}} # Reply with pid
    end
  end
end

defmodule Server.Database do
  use GenServer

  def start_link(opts) do
    server = Keyword.fetch!(opts, :name)
    GenServer.start_link(__MODULE__, server, opts)
  end

  @doc """
  Looks up the bucket pid for `name` stored in `table`.

  Returns `{:ok, pid}` if a bucket exists, `:error` otherwise.
  """
  def lookup(server, name) do
    # 2. lookup now expects a table and looks directly into ETS.
    #    No request is sent to the server.
    case :ets.lookup(server, name) do
      [{^name, pid}] -> {:ok, pid}
      [] -> :error
    end
  end

  @doc """
  Ensures there is a bucket associated with the given `name` in `server`.
  """

  def create(server, name) do
    GenServer.cast(server, {:create, name})
    #:ets.insert_new(tableName, datas)
  end

  def read(server,tableName,filter) do
    GenServer.cast(server, {:read, tableName, filter})
    #:ets.lookup(tableName, filter)
  end

  def delete(server, tableName) do
    GenServer.cast(server, {:delete, tableName})
    #:ets.delete(tableName, datas)
  end
  ## Server callbacks

  def init(table) do
    # 3. We have replaced the names HashDict by the ETS table
    names  = :ets.new(table, [:named_table, read_concurrency: true])
    refs = Map.new
    {:ok, {names, refs}}
  end

  # 4. The previous handle_call callback for lookup was removed

  def handle_cast({:create, name}, {names, refs}) do
    # 5. Read and write to the ETS table instead of the HashDict
    case lookup(names, name) do
      {:ok, _pid} ->
        {:noreply, {names, refs}}
      :error ->
        :ets.insert(names, {name})
        {:noreply, {names, refs}}
    end
  end

  def handle_cast({:read, name, filter}, _from) do
    # 5. Read and write to the ETS table instead of the HashDict
    :ets.lookup(name,filter)
  end

  def handle_cast({:delete, name}, _from) do
    # 5. Read and write to the ETS table instead of the HashDict
    :ets.delete(name)
  end

  def handle_info({:DOWN, ref, :process, _pid, _reason}, {names, refs}) do
    # 6. Delete from the ETS table instead of the HashDict
    {name, refs} = Map.pop(refs, ref)
    :ets.delete(names, name)
    {:noreply, {{names, name}, refs}}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

end

defmodule Server.Database do
  use GenServer
  require Server.JsonLoader

  def start_link(jsonfile, opts \\ []) do
    IO.puts(jsonfile)
    GenServer.start_link(__MODULE__, [jsonfile], opts)
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

  def search(database, criteria) do
    GenServer.call(database, {:search, criteria})
  end

  def handle_cast({:create, key, initial_value}, _from, table) do
    case :ets.insert_new(table, {key, initial_value}) do
    true -> {:reply, :ok, table}
    false -> {:reply, {:error, :keyalreadyexist}, table}
    end
  end

  def handle_call({:read, key}, _from, table) do
    case :ets.lookup(table, key) do
    [{^key, value}] -> {:reply, {:ok, value}, table}
    [] -> {:reply, {:error, :KeyNotFoud}, table}
    end
  end

  def handle_call({:update, key, values}, _from, table) do
    with [{^key, _val }] <- :ets.lookup(table, key),
    true <- :ets.insert_new(table, values) do
      {:reply, {:ok, values}}
    else
      [] -> {:reply, {:error, :KeyNotFoud}, table}
    end
  end

  def handle_cast({:delete, key}, _from, table) do
    with [{^key, _val }] <- :ets.lookup(table, key),
    true <- :ets.delete(table, key) do
      {:reply, {:ok, table}}
    else
      [] -> {:reply, {:error, :KeyNotFoud}, table}
    end
  end
  def create(server, name) do
    GenServer.call(server, {:create, name})
    #:ets.insert_new(tableName, datas)
  end

  ## Server callbacks

  # def init(jsonfile) do
  #   # 3. We have replaced the names HashDict by the ETS table
  #   names  = :ets.new(:table, [:named_table, read_concurrency: true])
  #   datas = Server.JsonLoader.load_to_database(:table,jsonfile)
  #   refs = Map.new
  #   {:ok, {names, refs}}
  # end

  def init(jsonfile) do
    # 3. We have replaced the names HashDict by the ETS table
    names  = :ets.new(:database, [:named_table, read_concurrency: true])
    {:ok, parsefile} = Server.JsonLoader.load_to_database(jsonfile)
    IO.inspect(parsefile)
    Enum.each(parsefile, fn element = %{order_id: order_id, items: items} when element <> %{order_id: nil, items: nil} ->
      case element do
        %{order_id: order_id, items: items}->
          :ets.insert_new(names, {order_id, items})
            # id = get_in(element, ["order_id"])
            # items = get_in(element, ["items"])|>Map.new()
            # {:id, :items} = {get_in(element, ["order_id"]), get_in(element, ["items"])}
            # if({nil, nil} != {:id, :items} )  do
              #if id != nil && items != nil do

              #IO.inspect(items)
      end
      #:ets.insert_new(names, {get_in(element, ["order_id"]), Map.new(get_in(element, ["items"]))})
      #|>IO.inspect({:id, :items})
    end)
    # Enum.each(refs, fn element ->
    #   IO.puts(element['id'])
    #   :ets.insert_new(names, {element['order_id'], {element['items'] }})
    # end)


    {:ok}
  end

  # 4. The previous handle_call callback for lookup was removed

  # def handle_call({:create, key, initial_value}, {names, refs}) do
  #   # 5. Read and write to the ETS table instead of the HashDict
  #   case lookup(names, name) do
  #     {:ok, _pid} ->
  #       {:noreply, {names, refs}}
  #     :error ->
  #       :ets.insert(names, {name})
  #       {:noreply, {names, refs}}
  #   end
  # end

  # def handle_call({:read, name, filter}, _from) do
  #   # 5. Read and write to the ETS table instead of the HashDict
  #   case :ets.lookup(name,filter) do
  #     [{^key, value}] -> {:reply, {:ok, value}, table}
  #     [] -> {:reply, {:error, value}, table}
  #   end
  # end

  # def handle_call({:delete, name}, _from) do
  #   # 5. Read and write to the ETS table instead of the HashDict
  #   :ets.delete(name)
  # end

  # def handle_info({:DOWN, ref, :process, _pid, _reason}, {names, refs}) do
  #   # 6. Delete from the ETS table instead of the HashDict
  #   {name, refs} = Map.pop(refs, ref)
  #   :ets.delete(names, name)
  #   {:noreply, {{names, name}, refs}}
  # end

  # def handle_info(_msg, state) do
  #   {:noreply, state}
  # end

  @doc """
  #criteria [{"key", "42"}, {"key", 42}] => liste d'id
  """
  # def search(database, criteria) do
  #    fun  = fn ({key, value}) -> do
  #      case :ets.lookup(database,{key, value}) do
  #        [{^key, value}] -> {:reply, {:ok, value}, table}
  #        [] -> {:reply, {:error, value}, table}
  #      end
  #    end

  #   case Enum.each(criteria, fun) do
  #     [{key, value}] -> {:reply, {:ok, [{key, value}]}, database}
  #     [] -> {:reply, {:error, criteria}, database}
  #   end

  # end

  def handle_call({:search, database, criteria}) do

   [{key , value}] =  :ets.match_object(:database, {:"$1", :_, :"$3"})

 end

    #search =  fn criteria, database -> list = Enum.flat_map(criteria, fn {k, v} -> Enum.filter(database, fn map ->  map[k] == v end) end) end
    #search =  fn criteria, database -> list = Enum.map(criteria, fn {​​​​​​​k, v}​​​​​​​ -> Enum.filter(database, fn map ->  map[k] == v end) end) |> List.flatten end

    #list = Enum.
#:ets.select(database, criteria)
#:ets.match()
#:ets.fun2ms(fn {id, key} )
    #retour
    #%{id, key}
end

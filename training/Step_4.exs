defmodule MyGenericServer do

  defp loop ({callback, state}) do
    receive do
      {:cast, msg}-> loop({callback,callback.handle_cast(msg,state)})
      {:call, msg,pid}->
        send(pid,callback.handle_call(msg,state))
        loop({callback,state})
    end
  end

  def cast(pid,msg), do: send(pid,{:cast,msg})
  def call(pid,msg) do
     send(pid,{:call,msg,self()})
     receive do msg-> msg end
  end

  def start_link(callback,state) do
    server_pid = spawn_link(fn-> loop({callback,state}) end)
    {:ok,server_pid}
  end
end

defmodule AccesEtsServer do

  def handle_cast({:credit, c}, amount), do: amount + c
  def handle_cast({:debit, c}, amount), do: amount - c
  def handle_call(:get, amount), do: amount

  def start_link(initial_amount) do
    MyGenericServer.start_link(AccesEtsServer,initial_amount)
  end

defmodule AccesEts do

  ## Client API

  @doc """
  Starts the registry with the given options.

  `:name` is always required.
  """
  def start_link(opts) do
    # 1. Pass the name to GenServer's init
    server = Keyword.fetch!(opts, :name)
    AccesEtsServer.start_link(__MODULE__, server, opts)
  end

  @doc """
  loop stanby a message
  """
  def loop() do
    receive do
    {:new, _, :table} ->  createNewTable({_, :table}

    end
  end

  #create
  #private

  def create_Insert_UpdateTable do


  end
  def createNewTable({:private, :set, :table}) do
    :ets.new(:table, [:table, :set, :private ])
  end

  def createNewTable({:private, :ordered_set, :table}) do
    :ets.new(:table, [:table, :ordered_set, :private ])
  end

  def createNewTable({:private, :bag, :table}) do
    :ets.new(:table, [:table, :bag, :private ])
  end

  def createNewTable({:private, _, :table}) do
    :ets.new(:table, [:table, :private ])
  end



  #public

  def createNewTable({:public, :set, :table}) do
    :ets.new(:table, [:table, :set, :public ])
  end

  def createNewTable({:public, :ordered_set, :table}) do
    :ets.new(:table, [:table, :ordered_set, :public ])
  end

  def createNewTable({:public, :bag, :table}) do
    :ets.new(:table, [:table, :bag, :public ])
  end

  def createNewTable({:public, _, :table}) do
    :ets.new(:table, [:table, :public ])
  end

  #protected

  def createNewTable({:protected, :set, :table}) do
    :ets.new(:table, [:table, :set, :protected ])
  end

  def createNewTable({:protected, :ordered_set, :table}) do
    :ets.new(:table, [:table, :ordered_set, :protected ])
  end

  def createNewTable({:protected, :bag, :table}) do
    :ets.new(:table, [:table, :bag, :protected ])
  end

  def createNewTable({:protected, _, :table}) do
    :ets.new(:table, [:table, :protected ])
  end

  #insert

  #get

  def getTable(:table) do
    :ets.table(:table)
  end
end

defmodule AccesEtsSupervisor do
  use Supervisor
  def init(initial_amount) do
    supervise([
      # more complex child spec, start_link is the default function to start Child, and second parameter is start_link parameter list
      # so here : AccountServer.start_link(initial_name) is called to start the child
      worker(AccesEtsServer,[initial_name])
    ], strategy: :one_for_one)
    # strategy is supervisor restart strategy :
    # one_for_one means when a single child dies, it is restarted alone
    # one_for_all means when a single child dies, all the other one are killed and restarted
  end
  def start_link(initial_name) do
    Supervisor.start_link(AccesEtsSupervisor,initial_name)
  end
end

defmodule AccountApp do
  use Application
  def start(_type,_args) do
    AccountSupervisor.start_link( # initial account credit hold is taken from the configuration system
      Application.get_env(:Step_4,:initial_account_amount))
  end
end

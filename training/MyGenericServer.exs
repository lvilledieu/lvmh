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

  def start(_type, _args) do
    import Supervisor.Spec
    children = [worker(Server.Database, [0])]
    opts = [strategy: :one_for_one]
    Supervisor.start_link(children, opts)
  end
end

defmodule AccountServer do
  def handle_cast({:credit, c}, amount), do: amount + c
  def handle_cast({:debit, c}, amount), do: amount - c
  def handle_call(:get, amount), do: amount

  def start_link(initial_amount) do
    MyGenericServer.start_link(AccountServer,initial_amount)
  end
end

{:ok, my_account} = AccountServer.start_link(4)
MyGenericServer.cast(my_account, {:credit, 5})
MyGenericServer.cast(my_account, {:credit, 2})
MyGenericServer.cast(my_account, {:debit, 3})
amount = MyGenericServer.call(my_account, :get)
IO.puts "current credit hold is #{amount}"

defmodule TutoKBRWStack do
  def start(_type, _args) do
    import Supervisor.Spec
    children = [worker(Server.Database, [0])]
    opts = [strategy: :one_for_one]
    Server.Serv_supervisor.start_link(children, opts)
  end
end

defmodule TutoKBRWStack do
  require Logger
  require Server.Router

  def start(_type, _args) do
    # file = IO.gets :stdio, "What is the location file please ?\n"

    # jsonfile = String.trim(file)

    import Supervisor.Spec
    children = [
      #{Server.Database, {jsonfile}, name: Server.Database},
      worker(Plug.Adapters.Cowboy.child_spec(:http, Server.Router, [], [port: 4001]))
     # ,{Plug.Cowboy, scheme: :http, plug: Server.TheFirstPlug, options: [port: 4001]}
    ]
    opts = [strategy: :one_for_one]
    Server.Serv_supervisor.start_link(children, opts)
  end
end

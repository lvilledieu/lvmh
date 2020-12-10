defmodule TutoKBRWStack do
  require Logger

  def start(_type, _args) do
    file = IO.gets "What is the location file please ? "

    jsonfile = String.trim(file)

    #import Supervisor.Spec
    children = [{Server.Database, {jsonfile}, name: Server.Database},
                {Plug.Cowboy, scheme: :http, plug: Example.HelloWorldPlug, options: [port: 8080]}]
    opts = [strategy: :one_for_one]
    Server.Serv_supervisor.start_link(children, opts)
  end
end

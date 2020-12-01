defmodule Serv_supervisor do
  use Supervisor
  def init(_) do
    children = [Server.Database]
    supervise(
        Enum.map(children, &worker(&1, [])),
        strategy: :one_for_one
        )
    end
    def start_link do
      {:ok, _} = Supervisor.start_link(__MODULE__, [], name: __MODULE__)
    end
end

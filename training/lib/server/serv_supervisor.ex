defmodule Server.Serv_supervisor do
  use Supervisor
  def init(children) do
    Supervisor.init(children, strategy: :one_for_one)
    end

    def start_link(children, opts) do
      {:ok, _} = Supervisor.start_link([children, name: __MODULE__], opts)
      #init(children)
    end
end

defmodule Server.Router do
  use Plug.Router

  plug :match
  plug :dispatch

  get "/" do
    send_resp(conn, 200, "Welcome to the new world of Plugs!")
  end

  get "/me" do
    send_resp(conn, 200, "I am The First, The One, Le Geant Plug Vert, Le Grand Plug, Le Plug Cosmique.")
  end

  match _ do
    send_resp(conn, 404, "Go away, you are not welcome here.")
  end
end

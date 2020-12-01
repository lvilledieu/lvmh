defmodule KV do
  @moduledoc """
  Documentation for KV.
  """

  @doc """
  Hello world.

  ## Examples

      iex> KV.hello()
      :world

  """

  use Application

  def start(_type, _args) do
    KV.Supervisor.start_link
  end

  def hello do
    :world
  end
end

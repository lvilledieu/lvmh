defmodule JsonLoader do

  def load_to_database(database, json_file) do
    #"files/" <> json_file
    File.read!(json_file)
    |>parse_json()

  end

  def convert_to_json(data) do
    Poison.encode(data)
  end

  def parse_json(json) do
    Poison.decode(json)
  end
end

defmodule Server.JsonLoader do
require Poison
  def load_to_database(json_file) do
    readfile = File.read!(json_file)
    {:ok, parsefile} = parse_json(readfile)
    {:ok, parsefile}
    #IO.inspect(get_in(parsefile, ["creation_date"]))
  end

  def convert_to_json(data) do
    Poison.encode(data)
  end

  def parse_json(json) do
    Poison.decode(json)
  end

end

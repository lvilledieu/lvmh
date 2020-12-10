defmodule Database_Test do
  use ExUnit.Case, async: true

  doctest(Server.Database)

  setup do
     filename = "files/orders_chunk1.json"
     database = Server.Database.start_link(filename)# Server.Database.start_link(filename, [])
     IO.inspect(database)
    {:ok, database: database}
  end

  test "start database with jsonfile" do
    #IO.inspect(database)
    # Server.Database.read()
    # assert KV.Bucket.get(bucket, "milk") == nil

    # KV.Bucket.put(bucket, "milk", 3)
    # assert KV.Bucket.get(bucket, "milk") == 3
  end
end

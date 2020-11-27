defmodule KVTest do
  use ExUnit.Case
  doctest KV

  test "greets the world" do
    assert KV.hello() == :world
  end

  test "the truth" do
    assert 1+1 == 2
  end
end

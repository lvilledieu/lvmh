defmodule TrainingTest do
  use ExUnit.Case
  doctest Training

  test "greets the world" do
    assert Training.hello() == :world
  end

  test "fails" do
    assert true == false
  end

end

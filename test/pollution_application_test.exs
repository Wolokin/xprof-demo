defmodule PollutionApplicationTest do
  use ExUnit.Case
  doctest PollutionApplication

  test "greets the world" do
    assert PollutionApplication.hello() == :world
  end
end

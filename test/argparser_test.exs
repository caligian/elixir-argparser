defmodule ArgparserTest do
  use ExUnit.Case

  doctest Argparser

  test "gets switch indices" do
    switches = %{
      "a" => %Switch{name: "a", n: 2},
      "b" => %Switch{name: "b", n: "*"},
      "c" => %Switch{name: "c", n: 3}
    }

    args = ["-1", "-2", "-a", "2", "3", "-b", "-c", "1"]

    Argparser.get_args(args, switches)
  end
end

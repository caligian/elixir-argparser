defmodule ArgparserTest do
  use ExUnit.Case
  import Argparser.DepsError
  import Argparser.WrongSpecError
  import Argparser.WrongNargsError

  doctest Argparser

  test "wrong nargs" do
    args = ["a", "b", "c", "--start", "python", "--with", "1", "2", "3",]
    specs = [%{long: "start", n: 2}, %{long: "with", n: "+"}]
    descs = {"header", "footer"}

    IO.inspect Argparser.parse(descs, specs, args)
    assert_raise WrongNargsError, fn -> Argparser.parse(descs, specs, args) end
  end

  test "strict args specification" do
    args =
      Enum.map(
        [
          "-a",
          "test/test",
          "-b",
          1,
          2,
          3,
          4,
          "-b",
          "-c",
          7,
          8,
          "-c",
          1,
          2,
          "-c",
          "1",
          "2",
          "3"
        ],
        fn x ->
          if not is_bitstring(x) do
            to_string(x)
          else
            x
          end
        end
      )

    specs = [
      %{
        name: "a",
        long: "long-name",
        n: 1,
        type: :contents,
        desc: "Hello world, teri maa ka bhosda"
      },
      %{name: "b", n: 4, type: :integer},
      %{name: "c", n: 7, type: :float},
      %{name: "d", default: fn -> :hello end}
    ]

    expected =
      {:ok,
       {
         [],
         %{
           b: [1, 2, 3, 4],
           c: [7.0, 8.0, 1.0, 2.0, 1.0, 2.0, 3.0],
           long_name: ["1\n"],
         }
       }}

    passed = Argparser.parse(
               {
                 "This is a test script",
                 "Well and this is another footer for you."
               },
               specs,
               args
             )

    IO.inspect passed

    assert expected == passed
  end

  test "greedy args specification" do
    args =
      Enum.map(
        [
          "-a",
          "test/test",
          "-b",
          1,
          2,
          3,
          4,
          "-b",
          "-c",
          7,
          8,
          "-c",
          1,
          2,
          "-c",
          "1",
          "2",
          "3",
          "--",
          "4",
          "5"
        ],
        fn x ->
          if not is_bitstring(x) do
            to_string(x)
          else
            x
          end
        end
      )

    specs = [
      %{
        name: "a",
        long: "long-name",
        n: "+",
        type: :contents,
        desc: "Hello world, teri maa ka bhosda"
      },
      %{name: "b", n: "+", type: :integer},
      %{name: "c", n: "*", type: :float},
      %{name: "d", default: fn -> :hello end}
    ]

    expected =
      {:ok,
       {
         ["4", "5"],
         %{
           b: [1, 2, 3, 4],
           c: [7.0, 8.0, 1.0, 2.0, 1.0, 2.0, 3.0],
           long_name: ["1\n"]
         }
       }}

    assert expected ==
             Argparser.parse(
               {
                 "This is a test script",
                 "Well and this is another footer for you."
               },
               specs,
               args
             )

    IO.inspect(expected)
  end
end



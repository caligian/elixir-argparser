defmodule Argparser.Help do
  alias Argparser.Help, as: Help

  def get_term_width() do
    case :io.columns() do
      {:ok, cols} ->  cols
      _ -> 30
    end
  end

  def create_body([], parsed) do
    parsed = 
      parsed
      |> Enum.reverse()
      |> Enum.map(fn x -> Enum.join(x, "") end)
      |> Enum.join("\n\n")

    IO.write parsed
  end

  def create_body([spec | rest], parsed) do
    max_width = Integer.floor_div(get_term_width(), 2)
    {name, name_size, desc} = spec

    strtimes = fn s, n ->
      Enum.map(1..n, fn _ -> s end)
    end

    whitespace = fn ->
      strtimes.(" ", max_width)
    end

    words = String.split(desc, ~r/\s+/)

    text =
      List.foldl(words, max_width, fn word, acc ->
        word_size = byte_size(word)

        case acc do
          n when is_number(n) ->
            case name_size do
              m when m + 2 > max_width ->
                [
                  {
                    max_width - word_size - 1,
                    List.flatten([name, "\n", whitespace.(), word, " "])
                  }
                ]

              _ ->
                remaining = max_width - name_size
                [{max_width - word_size - 1, [name, strtimes.(" ", remaining), word, " "]}]
            end

          _  ->
            {remaining, _} = List.last(acc)

            case remaining < word_size + 1 do
              true ->
                acc ++
                  [
                    {
                      max_width - 1 - word_size,
                      List.flatten(["\n", whitespace.(), word, " "])
                    }
                  ]

              false ->
                acc ++ [{remaining - 1 - word_size, [word, " "]}]
            end
        end
      end)

    text = Enum.map(text, fn {_, x} -> x end) |> List.flatten
    create_body(rest, [text | parsed])

  end

  def switch_to_string(spec) do
    desc = spec.desc
    metavar = spec.metavar || Atom.to_string(spec.type)
    n = spec.n
    required = spec.required
    is_flag = spec.n == 0

    metavar =
      case required do
        true -> "{#{metavar}}"
        false -> "[#{metavar}]"
      end

    metavar = 
      cond do
        is_bitstring(n) -> 
          "#{metavar}#{n}"

        true ->
          "#{metavar}{#{n}}"
      end

    name =
      cond do
        spec.name && spec.long ->
          if is_flag do
            "-#{spec.name}, --#{spec.long}"
          else
            "-#{spec.name} #{metavar}, --#{spec.long} #{metavar}"
          end

        spec.name and is_flag ->
          "-#{spec.name}"

        spec.long and is_flag ->
          "--#{spec.long}"

        spec.name ->
          "-#{spec.name} #{metavar}"

        spec.long ->
          "--#{spec.long} #{metavar}"
      end

    {name, String.length(name), desc}
  end
end

switch = %{
  n: 9,
  required: false,
  metavar: :string,
  name: "a",
  long: "a-test",
  desc: "madarchod ke bacche bhosdike. Teri maa ka bhosda. Laude ke baal gandu maa ki chut teri"
}

switches = [switch, switch]
switches = Enum.map(switches, &Argparser.Help.switch_to_string/1)
Argparser.Help.create_body(switches, [])



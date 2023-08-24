defmodule Switch do
  defstruct name: "",
            n: 0,
            pos: false,
            args: false,
            required: false,
            check: false,
            post: false,
            desc: "",
            sub: false
end

defmodule Argparser do
  defp pp(items) do
    IO.inspect(items)
  end

  defp slice(lst, from, till) do
    len = length(lst)

    cond do
      from >= till ->
        []

      from > len ->
        []

      till > len ->
        out = Enum.slice(lst, from, len - from)
        out

      true ->
        out = Enum.slice(lst, from, till - from)
        out
    end
  end

  def get_stdin() do
    IO.read(:stdio, :line)
    |> Enum.filter(fn x -> String.length(x) != 0 end)
    |> Enum.map(&String.trim(&1))
  end

  def get_switch_index(_args, [], parsed) do
    parsed
  end

  def get_switch_index(args, [current_switch | rest], parsed) do
    m = "-" <> current_switch.name
    pos = Enum.find_index(args, fn arg -> m == arg end)

    switches =
      if pos do
        Map.update!(parsed, current_switch.name, fn y -> %Switch{y | pos: pos} end)
      else
        parsed
      end

    get_switch_index(args, rest, switches)
  end

  defp get_switch_index(args, switches) do
    get_switch_index(args, Map.values(switches), switches)
  end

  defp get_pos_head(args, first_switch) do
    pos = first_switch.pos

    if pos > 0 do
      slice(args, 0, pos)
    else
      []
    end
  end

  defp wrong_nargs_error(switch, required, got) do
    throw({
      :wrong_nargs,
      [
        switch: switch,
        required: required,
        got: got
      ]
    })
  end

  # last_switch.args should not be computed at this point
  defp get_pos_tail(args, last_switch) do
    pos = last_switch.pos
    n = last_switch.n
    nargs = length(args)
    tail = slice(args, pos + 1, nargs)

    case n do
      "+" ->
        if pos != nargs - 1 do
          {%Switch{last_switch | args: tail}, []}
        else
          wrong_nargs_error(last_switch.name, n, 0)
        end

      "*" ->
        if pos != nargs - 1 do
          {
            %Switch{last_switch | args: tail},
            []
          }
        else
          {last_switch, []}
        end

      "?" ->
        if pos != nargs - 1 do
          {
            %Switch{last_switch | args: hd(tail)},
            tl(tail)
          }
        else
          {%Switch{last_switch | args: []}, tail}
        end

      n ->
        cond do
          n == 0 ->
            {%Switch{last_switch | args: []}, tail}

          pos + n + 1 > nargs ->
            wrong_nargs_error(last_switch, n, nargs - (pos + 1))

          true ->
            {
              %Switch{last_switch | args: slice(tail, 0, n)},
              :lists.nthtail(n, tail)
            }
        end
    end
  end

  defp parse_named_arg(_args, _x, false, acc) do
    acc
  end

  defp parse_named_arg(args, x, y, acc) do
    name1 = x.name
    pos1 = x.pos
    pos2 = y.pos
    n1 = x.n
    parsed_args = slice(args, pos1 + 1, pos2)
    parsed_nargs = length(parsed_args)
    parsed_args = (parsed_nargs == 0 && false) || parsed_args
    acc = Map.put(acc, name1, %Switch{x | args: parsed_args})

    cond do
      n1 == "+" or n1 == "*" ->
        if parsed_nargs == 0 and n1 == "+" do
          wrong_nargs_error(x, "+", 0)
        else
          acc
        end

      n1 == "?" ->
        if parsed_nargs != 1 or parsed_nargs != 0 do
          wrong_nargs_error(x, ~c"?", parsed_nargs)
        else
          acc
        end

      n1 != parsed_nargs ->
        wrong_nargs_error(x, n1, parsed_nargs)

      true ->
        acc
    end
  end

  defp get_named_args(args, switches, acc) do
    all_pos =
      Enum.map(0..(length(switches) - 1), fn i ->
        {Enum.at(switches, i), Enum.at(switches, i + 1) || false}
      end)

    List.foldl(all_pos, acc, fn {x, y}, acc ->
      parse_named_arg(args, x, y, acc)
    end)
  end

  def get_args(args, switches) do
    switches_with_pos = get_switch_index(args, switches)
    with_pos = Map.values(switches_with_pos) |> Enum.sort_by(fn x -> x.pos end)
    first_switch = hd(with_pos)
    last_switch = List.last(with_pos)
    pos_head = get_pos_head(args, first_switch)
    {last_switch, pos_tail} = get_pos_tail(args, last_switch)
    pos_args = pos_head ++ pos_tail

    named_args =
      get_named_args(args, with_pos, %{
        first_switch.name => first_switch,
        last_switch.name => last_switch
      })

    {pos_args, named_args}
  end

  defp validate_args(switch, parsed_args) do
  end
end

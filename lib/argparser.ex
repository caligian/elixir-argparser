defmodule Switch do
  defstruct name: "",
            n: 0,
            pos: false,
            args: false,
            dup: true,
            required: false,
            check: false,
            post: false,
            fail_on: [],
            desc: "",
            long: ""
end

defmodule WrongSpecError do
  defexception [:message]
end

defmodule WrongNargsError do
  defexception [:message]
end

defmodule Argparser do
  defp pp(args) do
    IO.inspect(args)
  end

  defp get_switch_pos([], _, pos) do
    pos
  end

  defp get_switch_pos(args, x, pos) do
    required = x.required
    len_name = String.length(x.name)
    len_long = String.length(x.long)
    nargs = length(args)
    offset = (length(pos) > 0 and List.last(pos) + 1) || 0

    cond do
      len_name > 0 and len_long > 0 ->
        m = Enum.find_index(args, &(&1 == "-" <> x.name))
        n = Enum.find_index(args, &(&1 == "--" <> x.long))

        cond do
          m && n ->
            args
            |> Enum.slice(((m > n && m) || n) + 1, nargs)
            |> get_switch_pos(x, Enum.sort([offset + m, offset + n] ++ pos))

          m ->
            args
            |> Enum.slice(m + 1, nargs)
            |> get_switch_pos(x, Enum.sort([offset + m | pos]))

          n ->
            args
            |> Enum.slice(n + 1, nargs)
            |> get_switch_pos(x, Enum.sort([offset + n | pos]))

          required ->
            raise WrongSpecError, switch: x, message: "x is required but not passed"

          true ->
            pos
        end

      len_name > 0 ->
        case Enum.find_index(args, &(&1 == x.name)) do
          n when is_number(n) ->
            args
            |> Enum.slice(n + 1, nargs)
            |> get_switch_pos(x, [offset + n | pos])

          _required when required ->
            raise WrongSpecError, switch: x, message: "switch is required but not passed"

          _ ->
            pos
        end

      len_long > 0 ->
        case Enum.find_index(args, &(&1 == x.long)) do
          n when is_number(n) ->
            args
            |> Enum.slice(n + 1, nargs)
            |> get_switch_pos(x, [offset + n | pos])

          _required when required ->
            raise WrongSpecError, switch: x, message: "switch is required but not passed"

          _ ->
            pos
        end

      true ->
        pos
    end
  end

  defp get_pos(_args, [], parsed) do
    Enum.sort_by(parsed, fn x -> x.pos end)
  end

  defp get_pos(args, [current | rest_switches], parsed) do
    pos = get_switch_pos(args, current, [])

    cond do
      is_list(pos) ->
        parsed =
          List.foldl(pos, parsed, fn x, acc ->
            [%Switch{current | pos: x} | acc]
          end)

        get_pos(args, rest_switches, parsed)

      is_number(pos) ->
        get_pos(args, rest_switches, [%Switch{current | pos: pos} | parsed])

      true ->
        get_pos(args, rest_switches, parsed)
    end
  end

  defp parse_switch_args(args, switch, next_switch) when is_number(next_switch) do
    %Switch{switch | args: Enum.slice(args, switch.pos + 1, length(args) - (switch.pos + 1))}
  end

  defp parse_switch_args(args, switch, next_switch) do
    next_switch_pos = next_switch.pos
    pos = switch.pos
    passed_nargs = next_switch_pos - (pos + 1)

    passed_args =
      cond do
        passed_nargs == 0 -> []
        true -> Enum.slice(args, pos + 1, passed_nargs)
      end

    %Switch{switch | args: passed_args}
  end

  defp long_or_short(switch) do
    len_long = String.length(switch.long)
    len_short = String.length(switch.name)

    cond do
      len_long > 0 and len_short > 0 ->
        switch.name

      len_long > 0 ->
        switch.long

      true ->
        switch.name
    end
  end

  defp check_switch_args(_args, [], _lookup, parsed) do
    Enum.reverse(parsed)
  end

  defp check_switch_args(args, [x], lookup, parsed) do
    is_zero_or_one = x.n == "?"

    case x.n do
      n when is_number(n) or is_zero_or_one ->
        prev_nargs = lookup[long_or_short(x)] || 0
        current_args = x.args || []
        current_nargs = length(current_args)
        total_nargs = prev_nargs + current_nargs

        cond do
          is_zero_or_one ->
            cond do
              prev_nargs > 0 and current_nargs > 0 ->
                if total_nargs > 1 do
                  raise WrongNargsError, inspect(switch: x, required: "0 or 1", got: total_nargs)
                else
                  check_switch_args(args, [], lookup, [
                    %Switch{x | args: [List.first(current_args)]} | parsed
                  ])
                end

              prev_nargs > 1 or current_nargs > 1 ->
                raise WrongNargsError, inspect(switch: x, required: "0 or 1", got: total_nargs)

              true ->
                check_switch_args(args, [], lookup, [
                  %Switch{x | args: [List.first(current_args)]} | parsed
                ])
            end

          prev_nargs == x.n ->
            check_switch_args(args, [], lookup, [%Switch{x | args: []} | parsed])

          total_nargs == x.n ->
            check_switch_args(args, [], lookup, [x | parsed])

          prev_nargs > x.n ->
            raise WrongNargsError, message: inspect(switch: x, required: x.n, got: total_nargs)

          total_nargs > x.n ->
            check_switch_args(args, [], lookup, [
              %Switch{x | args: Enum.slice(current_args, 0, abs(x.n - current_nargs))} | parsed
            ])

          true ->
            check_switch_args(args, [], lookup, [x | parsed])
        end

      _ ->
        check_switch_args(args, [], lookup, [x | parsed])
    end
  end

  defp check_switch_args(args, [switch | rest], lookup, parsed) do
    cond do
      switch.n == "+" or switch.n == "*" ->
        check_switch_args(args, rest, lookup, [switch | parsed])

      true ->
        name = long_or_short(switch)
        passed = switch.args || []
        exists = lookup[name] || 0
        n = exists + length(passed)
        requires_one_or_zero = switch.n == "?"
        requires_n = is_number(switch.n)
        lookup = Map.put(lookup, name, n)

        cond do
          requires_one_or_zero ->
            if n > 1 do
              raise WrongNargsError,
                switch: switch,
                message: "excess args",
                required: "0 or 1",
                passed: n
            else
              check_switch_args(args, rest, lookup, [switch | parsed])
            end

          requires_n && switch.n - n < 0 ->
            raise WrongNargsError,
              switch: switch,
              message: "excess args",
              required: switch.n,
              passed: n

          requires_n ->
            check_switch_args(args, rest, lookup, [switch | parsed])

          true ->
            check_switch_args(args, rest, lookup, [switch | parsed])
        end
    end
  end

  defp post_process_switches([], parsed) do
    parsed
  end

  defp post_process_switches([switch | rest], parsed) do
    check = switch.check

    post =
      switch.post &&
        fn x ->
          try do
            switch.post.(x)
          rescue
            e in ArgumentError ->
              raise WrongSpecError, switch: switch, message: inspect(e)
          else
            out -> out
          end
        end

    cond do
      check and post ->
        out = post.(switch.args)

        if not check.(out) do
          raise WrongSpecError, switch: switch, message: "switch failed validation"
        else
          post_process_switches(rest, [%Switch{switch | args: out} | parsed])
        end

      check ->
        if not check.(switch.args) do
          raise WrongSpecError, switch: switch, message: "switch failed validation"
        else
          post_process_switches(rest, [switch | parsed])
        end

      post ->
        post_process_switches(rest, [%Switch{switch | args: post.(switch.args)} | parsed])

      true ->
        post_process_switches(rest, [switch | parsed])
    end
  end

  defp extract_args(args, [x], parsed) do
    with_args = parse_switch_args(args, x, length(args))
    [with_args | parsed]
  end

  defp extract_args(args, [current | rest], parsed) do
    with_args = parse_switch_args(args, current, hd(rest))
    extract_args(args, rest, [with_args | parsed])
  end

  defp get_named_args([], map) do
    List.foldl(Map.keys(map), %{}, fn {i, x}, acc ->
      v = map[{i, x}]
      Map.update(acc, x, v, fn lst -> lst ++ v end)
    end)
  end

  defp get_named_args([current | rest], map) do
    name =
      case current.name do
        "" -> current.long
        _ -> current.name
      end

    n = current.n
    prev_args = map[{n, name}]
    dup = current.dup

    args =
      case prev_args do
        nil ->
          case current.args do
            nil -> false
            _ -> current.args
          end

        _ ->
          prev_args ++ (current.args || [])
      end

    cond do
      prev_args && not dup ->
        raise WrongSpecError, message: [switch: current, reason: :duplicate]

      true ->
        get_named_args(rest, Map.put(map, {n, name}, args))
    end
  end

  def parse_args(args, spec) do
    parsed = get_pos(args, Map.values(spec), [])
    parsed = extract_args(args, parsed, [])
    parsed = check_switch_args(args, Enum.reverse(parsed), %{}, [])
    parsed = post_process_switches(parsed, [])
    parsed_map = get_named_args(Enum.reverse(parsed), %{})
    last_pos = List.first(parsed)
    first_pos = List.last(parsed)

    last_pos =
      if last_pos.args && length(last_pos.args) > 0 do
        last_pos.pos + length(last_pos.args) + 1
      else
        last_pos.pos + 1
      end

    pos_head = Enum.slice(args, 0, first_pos.pos)
    pos_tail = Enum.slice(args, last_pos, length(args) - last_pos)
    pos_args = pos_head ++ pos_tail

    {pos_args, parsed_map}
  end

  def test() do
    args = [
      1,
      2,
      3,
      4,
      "-a",
      1,
      "--long-name",
      "-a",
      1,
      "-b",
      1,
      "--b-long-name",
      2,
      3,
      4,
      "-b",
      6,
      7,
      8,
      -11
    ]

    spec = %{
      "a" => %Switch{name: "a", long: "long-name", n: "?"},
      "b" => %Switch{name: "b", long: "b-long-name", n: "+"}
    }

    pp(parse_args(args, spec))
  end
end



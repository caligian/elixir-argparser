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
  defexception [:switch, :message]
end

defmodule WrongNargsError do
  defexception [:switch, :message, :passed, :required]
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

  defp parse_switch_args(args, switch, next_switch) do
    next_switch_pos =
      case next_switch do
        x when is_number(x) -> x
        _ -> next_switch.pos
      end

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
        switch.long

      len_long > 0 ->
        switch.long

      true ->
        switch.name
    end
  end

  defp check_switch_args([], _lookup) do
    :ok
  end

  defp check_switch_args([switch | rest], lookup) do
    cond do
      switch.n == "+" or switch.n == "+" ->
        check_switch_args(rest, lookup)

      true ->
        name = long_or_short(switch)
        passed = switch.args || []
        n = length(passed)
        exists = lookup[name] || false
        requires_one_or_zero = switch.n == "?"
        requires_n = is_number(switch.n)

        cond do
          requires_one_or_zero && n > 1 ->
            raise WrongNargsError,
              switch: switch,
              message: "excess args",
              required: "0 or 1",
              passed: n

          requires_n && switch.n - (exists || n) < 0 ->
            raise WrongNargsError,
              switch: switch,
              message: "excess args",
              required: switch.n,
              passed: n

          requires_n && exists ->
            check_switch_args(rest, Map.put(lookup, name, n + exists))

          requires_n ->
            check_switch_args(rest, Map.put(lookup, name, n))

          true ->
            check_switch_args(rest, lookup)
        end
    end
  end

  defp post_process_switches([], parsed) do
    parsed
  end

  defp post_process_switches([switch | rest], parsed) do
    check = switch.check
    post = switch.post

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
    with_args = parse_switch_args(args, current, hd(rest).pos)
    extract_args(args, rest, [with_args | parsed])
  end

  defp get_named_args_map([], map) do
    List.foldl(Map.keys(map), %{}, fn {required_n, x}, acc ->
      k = String.to_atom(String.replace(x, "-", "_"))
      v = map[{required_n, x}]
      n = length(v)

      cond do
        is_number(required_n) and required_n != n ->
          raise WrongNargsError, switch: x, message: "expected #{required_n}, got #{n}"

        true ->
          Map.put(acc, k, v)
      end
    end)
  end

  defp get_named_args_map([current | rest], map) do
    name = current.name
    n = current.n

    cond do
      not Map.has_key?(map, {n, name}) and not Map.has_key?(map, {n, current.long}) ->
        map = Map.put(map, {current.n, long_or_short(current)}, current.args || [])
        get_named_args_map(rest, map)

      current.n == 0 ->
        throw({:duplicate, current})

      current.dup ->
        map =
          Map.update!(map, {current.n, long_or_short(current)}, fn lst ->
            lst ++ (current.args || [])
          end)

        get_named_args_map(rest, map)
    end
  end

  def parse_args(args, spec) do
    parsed = get_pos(args, Map.values(spec), [])
    parsed = extract_args(args, parsed, [])
    :ok = check_switch_args(parsed, %{})
    parsed = post_process_switches(parsed, [])
    parsed = get_named_args_map(parsed, %{})
    pp(parsed)
  end

  def test() do
    args = [
      1,
      2,
      3,
      4,
      "-a",
      1,
      2,
      3,
      "--long-name",
      4,
      5,
      "-a",
      "-1",
      "-b",
      1,
      "--b-long-name",
      2,
      3,
      4
    ]

    spec = %{
      "a" => %Switch{name: "a", long: "long-name", n: "+"},
      "b" => %Switch{name: "b", long: "b-long-name", n: "*"}
    }

    parse_args(args, spec)
  end
end



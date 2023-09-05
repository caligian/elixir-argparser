defmodule Switch do
  defstruct name: false,
            n: 0,
            metavar: :string,
            pos: false,
            args: false,
            dup: true,
            required: false,
            check: false,
            post: false,
            without: [],
            deps: [],
            desc: "",
            long: false,
            default: false,
            type: :string
end

defmodule DepsError do
  defexception [:message]
end

defmodule WrongSpecError do
  defexception [:message]
end

defmodule WrongNargsError do
  defexception [:message]
end

defmodule Argparser do
  @doc """

  """

  @type argv() :: [str()]
  @type switch() :: %Switch{}
  @type specs() :: [switch()]
  @type pos_args() :: list(String.t())
  @type named_args() :: %{String.t() => any()}
  @type parsed_args() :: {pos_args(), named_args()}
  @type str() :: String.t()

  @spec get_stdin() :: [str()] | :error
  def get_stdin(sep \\ ~r"[\n\r]") do
    case IO.read(:stdio, :all) do
      {:error, _} -> :error
      s -> s |> String.split(sep) |> Enum.filter(fn x -> String.length(x) > 0 end)
    end
  end

  @spec pp(any()) :: any()
  defp pp(args) do
    IO.inspect(args)
    args
  end

  defp get_desc(short, specs) when is_bitstring(short) do
    get_desc([short, ""], specs)
  end

  # TODO
  defp get_desc([short, long], specs) do
    script_name = String.replace __ENV__.file, System.get_env("HOME"), "~"

    print_metavar = fn type, n, required ->
      case n do
        "*" -> "[#{type}, [#{type}, [...]]]"
        0 -> ""
        1 when required -> "{#{type}}"
        y when y == 1 or y == "?" -> "[#{type}]"
        "+" -> "#{type}, [#{type}, [...]]"
        _ -> "#{type}, {#{type}, [...]}"
      end
    end

    print_switch = fn x ->
      metavars = print_metavar.(x.metavar, x.n, x.required)

      cond do
        x.name && x.long ->
          "-#{x.name} | --#{x.long}: #{metavars}"

        x.name ->
          "-#{x.name}: #{metavars}"

        x.long ->
          "--#{x.long}: #{metavars}"
      end
    end

    [
      "#{script_name}: #{short}",
      case long do
        "" -> ""
        _ -> [long, ""]
      end
      | Enum.map(specs, fn x ->
          [
            print_switch.(x),
              case x.n do
                0 -> "Is a flag?: true"
                _ -> "Requires N: #{x.n}"
              end,
            "Required: #{x.required}",
            "Supports duplicate instances: #{x.dup}",
            "To be used without: " <>
              case x.without do
                [] -> "none"
                lst -> List.to_string(lst)
              end,
            "To be used with: " <>
              case x.deps do
                [] -> "none"
                lst -> List.to_string(lst)
              end,
            "Argument type: #{x.type}",
            "Description:",
            x.desc,
            ""
          ]
        end)
    ]
    |> List.flatten()
    |> Enum.join("\n")
  end

  def print_help(long_desc, specs) do
    IO.puts(get_desc(long_desc, specs))
  end

  @spec validate_names(switch()) :: nil
  def validate_names(switch) do
    name = switch.name
    long = switch.long
    regex = ~r/^[a-zA-Z0-9][a-zA-Z0-9-]*$/

    check = fn x ->
      unless Regex.run(regex, x) do
        raise WrongSpecError,
          message:
            "#{long_or_short(x)}: switch names can only contain alphanumeric characters and/or '-' "
      end
    end

    cond do
      name && long ->
        check.(name)
        check.(long)

      name ->
        check.(name)

      long ->
        check.(long)
    end
  end

  def get_all_pos([], _, pos) do
    pos
  end

  @spec get_all_pos(argv(), switch(), list(number())) :: list(switch())
  def get_all_pos(args, x, pos) do
    validate_names(x)

    required = x.required
    nargs = length(args)
    offset = (length(pos) > 0 and List.last(pos) + 1) || 0

    cond do
      x.name && x.long ->
        m = Enum.find_index(args, &(&1 == "-" <> x.name))
        n = Enum.find_index(args, &(&1 == "--" <> x.long))

        cond do
          m && n ->
            args
            |> Enum.slice(((m > n && m) || n) + 1, nargs)
            |> get_all_pos(x, Enum.sort([offset + m, offset + n] ++ pos))

          m ->
            args
            |> Enum.slice(m + 1, nargs)
            |> get_all_pos(x, Enum.sort([offset + m | pos]))

          n ->
            args
            |> Enum.slice(n + 1, nargs)
            |> get_all_pos(x, Enum.sort([offset + n | pos]))

          required ->
            raise WrongSpecError, message: "#{long_or_short(x)}: required but not passed"

          true ->
            pos
        end

      x.name ->
        case Enum.find_index(args, &(&1 == "-" <> x.name)) do
          n when is_number(n) ->
            args
            |> Enum.slice(n + 1, nargs)
            |> get_all_pos(x, [offset + n | pos])

          _required when required ->
            raise WrongSpecError,
              message: " #{long_or_short(x)} switch is required but not passed"

          _ ->
            pos
        end

      x.long ->
        case Enum.find_index(args, &(&1 == "--" <> x.long)) do
          n when is_number(n) ->
            args
            |> Enum.slice(n + 1, nargs)
            |> get_all_pos(x, [offset + n | pos])

          _required when required ->
            raise WrongSpecError, message: "#{long_or_short(x)} switch is required but not passed"

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

  @spec get_pos(argv(), [switch()], [switch()]) :: [switch()]
  defp get_pos(args, [current | rest_switches], parsed) do
    pos = get_all_pos(args, current, [])

    case pos do
      [] ->
        get_pos(args, rest_switches, parsed)

      _ ->
        parsed =
          List.foldl(pos, parsed, fn x, acc ->
            [%Switch{current | pos: x} | acc]
          end)

        get_pos(args, rest_switches, parsed)
    end
  end

  @spec parse_switch_args(argv(), switch(), number()) :: switch()
  defp parse_switch_args(args, switch, next_switch) when is_number(next_switch) do
    %Switch{switch | args: Enum.slice(args, switch.pos + 1, length(args) - (switch.pos + 1))}
  end

  @spec parse_switch_args(argv(), switch(), switch()) :: switch()
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

  @spec long_or_short(switch()) :: str()
  defp long_or_short(switch) do
    cond do
      switch.name && switch.long ->
        switch.long

      switch.long ->
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
      n when is_number(n) or is_zero_or_one or n == "+" ->
        prev_nargs = lookup[long_or_short(x)] || 0
        current_args = x.args || []
        current_nargs = length(current_args)
        total_nargs = prev_nargs + current_nargs

        cond do
          n == "+" ->
            if total_nargs < 1 do
              raise WrongNargsError,
                message:
                  "#{long_or_short(x)} expected more than equal to 1 arg[s], got #{total_nargs}"
            else
              check_switch_args(args, [], lookup, [x | parsed])
            end

          is_zero_or_one ->
            cond do
              prev_nargs > 0 and current_nargs > 0 ->
                if total_nargs > 1 do
                  raise WrongNargsError,
                    message: "#{long_or_short(x)} 0 or 1 required, got #{total_nargs}"
                else
                  check_switch_args(args, [], lookup, [
                    %Switch{x | args: [List.first(current_args)]} | parsed
                  ])
                end

              prev_nargs > 1 or current_nargs > 1 ->
                raise WrongNargsError,
                  message: "#{long_or_short(x)} 0 or 1 required, got #{total_nargs}"

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
            raise WrongNargsError,
              message: "#{long_or_short(x)}: required #{x.n}, got #{total_nargs}"

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

  @spec check_switch_args(argv(), [switch()], map(), list(switch())) :: list(switch())
  defp check_switch_args(args, [switch | rest], lookup, parsed) do
    cond do
      switch.n == "*" ->
        check_switch_args(args, rest, lookup, [switch | parsed])

      true ->
        name = long_or_short(switch)
        passed = switch.args || []
        exists = lookup[name] || 0
        n = exists + length(passed)
        requires_n = is_number(switch.n)
        lookup = Map.put(lookup, name, n)

        cond do
          switch.n == "+" ->
            if n < 1 do
              raise WrongNargsError, message: "#{long_or_short(switch)} 0 or 1 required, got #{n}"
            else
              check_switch_args(args, rest, lookup, [switch | parsed])
            end

          switch.n == "?" ->
            if n > 1 do
              raise WrongNargsError,
                message: "#{long_or_short(switch)}: required 0 or 1, got #{n}"
            else
              check_switch_args(args, rest, lookup, [switch | parsed])
            end

          requires_n && switch.n - n < 0 ->
            raise WrongNargsError,
              message: "#{long_or_short(switch)}: required #{switch.n}, got #{n}"

          requires_n ->
            check_switch_args(args, rest, lookup, [switch | parsed])

          true ->
            check_switch_args(args, rest, lookup, [switch | parsed])
        end
    end
  end

  @spec get_switch_from_spec([switch()], str()) :: switch() | false
  defp get_switch_from_spec(spec, name) do
    name = (is_atom(name) && Atom.to_string(name)) || name
    name = String.replace(name, "_", "-")
    switch = Enum.filter(spec, fn x -> x.name == name or x.long == name end)

    Enum.at(switch, 0) || false
  end

  @spec post_process_switches(specs(), named_args()) :: list(switch())
  defp post_process_switches(spec, parsed_map) do
    List.foldl(Map.keys(parsed_map), parsed_map, fn switch, acc ->
      name = switch
      switch = get_switch_from_spec(spec, name)
      check = switch.check
      type = switch.type
      args = parsed_map[name]

      args =
        cond do
          is_list(args) ->
            case type do
              :atom ->
                Enum.map(args, &String.to_atom(&1))

              :float ->
                Enum.map(args, fn x ->
                  {n, _} = Float.parse(x)
                  n
                end)

              :integer ->
                Enum.map(args, fn x ->
                  {n, _} = Integer.parse(x)
                  n
                end)

              :regex ->
                Enum.map(args, &Regex.compile(&1))

              :file ->
                Enum.each(args, fn x ->
                  case File.exists?(x) do
                    false -> throw({:invalid_file_path, x})
                    true -> x
                  end
                end)

              :dir ->
                Enum.each(args, fn x ->
                  case File.dir?(x) do
                    false -> throw({:invalid_dir_path, x})
                    true -> x
                  end
                end)

              :contents ->
                Enum.map(args, fn x ->
                  case File.exists?(x) do
                    false ->
                      throw({:invalid_dir_path, x})

                    true ->
                      out = File.read!(x)
                      out
                  end
                end)

              _ ->
                args
            end

          true ->
            args
        end

      post =
        switch.post &&
          fn x ->
            try do
              switch.post.(x)
            rescue
              _ in ArgumentError ->
                raise WrongSpecError,
                  message: "#{long_or_short(switch)}: switch failed post processing"
            else
              out -> out
            end
          end

      cond do
        check && post ->
          out = post.(args)

          if not check.(out) do
            raise WrongSpecError, message: "#{name}: switch failed validation"
          else
            Map.put(acc, name, out)
          end

        check ->
          if not check.(args) do
            raise WrongSpecError, message: "#{name}: switch failed validation"
          else
            Map.put(acc, name, args)
          end

        post ->
          Map.put(acc, name, post.(args))

        true ->
          Map.put(acc, name, args)
      end
    end)
  end

  defp extract_args(_, [], parsed) do
    Enum.reverse(parsed)
  end

  defp extract_args(args, [x], parsed) do
    with_args = parse_switch_args(args, x, length(args))
    Enum.reverse([with_args | parsed])
  end

  @spec extract_args(argv(), [switch()], [switch()]) :: [switch()]
  defp extract_args(args, [current | rest], parsed) do
    with_args = parse_switch_args(args, current, hd(rest))
    extract_args(args, rest, [with_args | parsed])
  end

  defp get_named_args([], map) do
    List.foldl(Map.keys(map), %{}, fn x, acc ->
      name = x |> String.replace("-", "_") |> String.to_atom()
      Map.put(acc, name, map[x])
    end)
  end

  @spec get_named_args([switch()], named_args()) :: named_args()
  defp get_named_args([current | rest], map) do
    name = long_or_short(current)
    prev_args = map[name] || []
    dup = current.dup

    cond do
      prev_args && not dup ->
        raise WrongSpecError, message: "#{current} cannot pass more than instance of this switch"

      true ->
        args = prev_args ++ (current.args || [])
        get_named_args(rest, Map.put(map, name, args))
    end
  end

  defp check_deps([], map) do
    List.foldl(Map.keys(map), %{}, fn x, acc ->
      v = (length(map[x]) > 0 and map[x]) || true
      Map.put(acc, x, v)
    end)
  end

  defp check_deps(specs, map) do
    List.foldl(Map.keys(map), %{}, fn x, _ ->
      switch = get_switch_from_spec(specs, x)

      cond do
        switch == false ->
          false

        switch ->
          case switch.deps do
            [] ->
              true

            d ->
              case Enum.filter(d, fn y -> get_switch_from_spec(specs, y) end) do
                [] -> raise WrongSpecError, message: "#{x}: missing deps: #{d}"
                _ -> true
              end
          end

          case switch.without do
            [] ->
              true

            w ->
              found = Enum.filter(w, fn y -> get_switch_from_spec(specs, y) end)

              case length(found) do
                n when n > 0 ->
                  raise WrongSpecError, message: "#{x}: unneeded deps found: #{found}"

                _ ->
                  true
              end
          end
      end
    end)
  end

  def parse_args([]) do
    false
  end

  defp to_list(x) do
    if not is_list(x) do
      [x]
    else
      x
    end
  end

  @spec get_default_args([switch()], named_args()) :: named_args()
  def get_default_args(spec, parsed_map) do
    List.foldl(spec, parsed_map, fn x, acc ->
      name = long_or_short(x)

      cond do
        x.default && not Map.has_key?(parsed_map, name) ->
          Map.put(
            acc,
            String.replace(name, "_", "-") |> String.to_atom(),
            x.default.() |> to_list
          )

        true ->
          acc
      end
    end)
  end

  @spec parse!(str(), specs()) :: parsed_args()
  def parse!(long_desc, spec) do
    parse(long_desc, spec, System.argv())
  end

  def extract_till_sep(args) do
    sep_pos = Enum.find_index(args, fn x -> x == "--" end) || -1

    cond do
      sep_pos == -1 ->
        {{}, args}

      sep_pos ->
        {
          Enum.slice(args, 0, sep_pos),
          Enum.slice(args, sep_pos + 1, length(args) - sep_pos)
        }
    end
  end

  def parse(_, _, []) do
    false
  end

  def parse(_, [], args) do
    {args, false}
  end

  @spec parse(str(), specs(), argv()) :: parsed_args()
  def parse(long_desc, spec, args) do
    cond do
      Enum.find_index(args, fn x -> x == "--help" end) ->
        print_help(long_desc, spec)

      true ->
        {tail, args} = extract_till_sep(args)
        parsed = get_pos(args, spec, [])
        parsed = extract_args(args, parsed, [])
        parsed = check_switch_args(args, parsed, %{}, [])
        parsed_map = get_named_args(parsed, %{})
        parsed_map = post_process_switches(spec, parsed_map)
        parsed_map = get_default_args(spec, parsed_map)
        _ = check_deps(parsed, parsed_map)
        last_pos = List.last(parsed)
        first_pos = List.first(parsed)

        last_pos =
          if last_pos do
            if last_pos.args && length(last_pos.args) > 0 do
              last_pos.pos + length(last_pos.args) + 1
            else
              last_pos.pos + 1
            end
          end

        pos_head =
          if first_pos do
            Enum.slice(args, 0, first_pos.pos)
          else
            []
          end

        pos_tail =
          if last_pos do
            Enum.slice(args, last_pos, length(args) - last_pos)
          else
            []
          end

        parsed_map =
          if map_size(parsed_map) == 0 do
            false
          else
            List.foldl(Map.keys(parsed_map), %{}, fn x, acc ->
              out =
                case parsed_map[x] do
                  [] -> true
                  [item] -> item
                  v -> v
                end

              Map.put(acc, x, out)
            end)
          end

        pos_tail = pos_tail ++ tail
        pos_args = pos_head ++ pos_tail

        if length(pos_args) > 0 do
          {pos_args, parsed_map}
        else
          {false, parsed_map}
        end
    end
  end

  def test() do
    args =
      Enum.map(
        [
          "--help",
          1,
          2,
          3,
          4,
          "-a",
          1,
          "/home/caligian/.bashrc",
          "-b",
          1,
          2,
          3,
          4,
          "-b",
          6,
          7,
          8,
          -11,
          "-c",
          1
        ],
        fn x ->
          (is_integer(x) && Integer.to_string(x)) || x
        end
      )

    specs = [
      %Switch{
        name: "a",
        long: "long-name",
        n: 5,
        metavar: :path,
        type: :contents,
        desc: "Hello world, teri maa ka bhosda"
      },
      %Switch{name: "b", n: "+", type: :integer},
      %Switch{name: "c", n: "+", type: :float},
      %Switch{name: "d", default: fn -> :hello end}
    ]

    parse(["hello world!", "some long description"], specs, args)
  end
end

Argparser.test()



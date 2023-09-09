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
  @type specs() :: [map()]
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

  # @spec pp(any()) :: any()
  # defp pp(args) do
  #   IO.inspect(args)
  #   args
  # end

  defp get_default() do
    %{
      name: false,
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
    }
  end

  @spec add_defaults(map()) :: map()
  defp add_defaults(x) when is_map(x) do
    Map.keys(x) |> List.foldl(get_default(), &Map.put(&2, &1, x[&1]))
  end

  @spec add_defaults(list(map())) :: list(map())
  defp add_defaults(xs) do
    Enum.map(xs, &add_defaults/1)
  end

  defp get_desc(short, specs) when is_bitstring(short) do
    get_desc({short, ""}, specs)
  end

  # TODO
  defp get_desc({short, long}, specs) do
    script_name = String.replace(__ENV__.file, System.get_env("HOME"), "~")

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

  @spec validate_names(map()) :: nil
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

  defp find_index([], _, _, x, found) do
    found |> List.flatten() |> Enum.reverse() |> Enum.map(fn pos -> %{x | pos: pos} end)
  end

  defp find_index(xs, full_len, offset, x, found) do
    name = Map.get(x, :name)
    long = Map.get(x, :long)
    name_pos = name && Enum.find_index(xs, fn y -> "-" <> name == y end)
    long_pos = long && Enum.find_index(xs, fn y -> "--" <> long == y end)

    pos =
      cond do
        name_pos && long_pos -> [name_pos, long_pos]
        name_pos -> [name_pos]
        long_pos -> [long_pos]
        true -> false
      end

    case pos do
      false ->
        find_index([], full_len, offset, x, found)

      _ ->
        pos = Enum.sort(pos)
        len = length(xs)
        pos_len = length(pos)
        last = List.last(pos)
        offset = full_len - len
        put_pos = offset + last

        cond do
          pos_len == 0 ->
            find_index([], full_len, offset, x, found)

          pos_len == len - 1 ->
            find_index([], full_len, offset, x, [put_pos | found])

          true ->
            check = List.last(pos)
            remaining = Enum.slice(xs, check + 1, len - (check + 1))
            find_index(remaining, full_len, len, x, [put_pos | found])
        end
    end
  end

  def find_index(args, xs) do
    xs
    |> Enum.map(fn x -> find_index(args, length(args), 0, x, []) end)
    |> Enum.filter(& &1)
    |> List.flatten()
    |> Enum.sort_by(& &1.pos)
  end

  @spec parse_switch_args(argv(), map(), number()) :: map()
  defp parse_switch_args(args, switch, next_switch) when is_number(next_switch) do
    args_len = length(args)
    args_left = args_len - (switch.pos + 1)
    remaining_args = Enum.slice(args, switch.pos + 1, args_left)

    %{switch | args: remaining_args}
  end

  @spec parse_switch_args(argv(), map(), map()) :: map()
  defp parse_switch_args(args, switch, next_switch) do
    next_switch_pos = next_switch.pos
    pos = switch.pos
    passed_nargs = next_switch_pos - (pos + 1)

    passed_args =
      cond do
        passed_nargs == 0 -> []
        true -> Enum.slice(args, pos + 1, passed_nargs)
      end

    %{switch | args: passed_args}
  end

  @spec long_or_short(map()) :: str()
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
    {last, args_tail} = hd(parsed)
    {Enum.reverse([last | tl(parsed)]), args_tail}
  end

  defp check_switch_args(args, [x], lookup, parsed) do
    n = x.n
    pos = x.pos
    name = long_or_short(x)
    prev_nargs = lookup[name] || 0
    current_args = x.args || []
    current_nargs = length(current_args)
    total_nargs = prev_nargs + current_nargs
    args_len = length(args)
    remaining = args_len - (pos + 1)

    remaining_args =
      case remaining do
        0 -> []
        _ -> Enum.slice(args, pos + 1, remaining)
      end

    res =
      cond do
        n == "+" ->
          if total_nargs < 1 do
            raise WrongNargsError,
              message: "#{name}: expected more than equal to 1 arg[s], got #{total_nargs}"
          else
            remaining_args
          end

        n == "*" ->
          remaining_args

        n == "?" ->
          (remaining > 0 && hd(remaining_args)) || []

        is_number(n) ->
          cond do
            prev_nargs == n ->
              []

            total_nargs == n ->
              remaining_args

            n < prev_nargs ->
              raise WrongNargsError,
                message:
                  "#{name}: too many arguments. required #{n}, got #{current_nargs + prev_nargs}"

            n < current_nargs ->
              Enum.slice(current_args, 0, n)

            true ->
              []
          end

        true ->
          raise WrongSpecError, message: "#{name}: need any of [0-9]+ or [+*?], got #{n}"
      end

    res = to_list(res)
    res_len = length(res) + prev_nargs

    if is_number(n) and res_len != n do
      raise WrongNargsError, message: "#{name}: too few arguments. required #{n}, got #{res_len}"
    end

    args_tail =
      case length(res) do
        0 -> remaining_args
        n when n == remaining -> []
        n -> Enum.slice(remaining_args, n, remaining - n)
      end

    x = {%{x | args: res}, args_tail}

    check_switch_args(args, [], lookup, [x | parsed])
  end

  @spec check_switch_args(argv(), [map()], map(), list(map())) :: list(map())
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

  @spec get_switch_from_spec([map()], str()) :: map() | false
  defp get_switch_from_spec(spec, name) do
    name = (is_atom(name) && Atom.to_string(name)) || name
    name = String.replace(name, "_", "-")
    switch = Enum.filter(spec, fn x -> x.name == name or x.long == name end)

    Enum.at(switch, 0) || false
  end

  @spec post_process_switches(specs(), named_args()) :: list(map())
  defp post_process_switches(spec, parsed_map) do
    List.foldl(Map.keys(parsed_map), parsed_map, fn switch, acc ->
      name = switch
      switch = get_switch_from_spec(spec, name)
      check = switch.check
      type = switch.type
      args = parsed_map[name]
      len = length(args)

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

              _ when type ==  :integer or type == :int ->
                Enum.map(args, fn x ->
                  {n, _} = Integer.parse(x)
                  n
                end)

              :regex ->
                Enum.map(args, &Regex.compile!(&1))

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
                    false -> throw({:invalid_file_path, x})
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

              :string -> args
              :str -> args

              _ ->
                raise "#{name}: invalid type: #{type}"
            end

          true ->
            args
        end

      args =
        if switch.post do
          Enum.map(to_list(args), switch.post)

          res =
            case type do
              0 -> true
              1 -> hd(args)
              "?" when len > 0 -> hd(args)
              "?" when len == 0 -> true
              "*" when len == 0 -> true
              _ -> args
            end

          res
        else
          args
        end

      if check do
        correct? = Enum.all?(Enum.map(to_list(args), check))

        cond do
          not correct? ->
            raise WrongSpecError, message: "#{name}: switch failed validation"

          true ->
            Map.put(acc, name, args)
        end
      else
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

  @spec extract_args(argv(), [map()], [map()]) :: [map()]
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

  @spec get_named_args([map()], named_args()) :: named_args()
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
              found =
                Enum.filter(d, fn x ->
                  case x do
                    y when is_list(y) ->
                      res = Enum.filter(y, fn z -> get_switch_from_spec(specs, z) end)

                      if length(res) != length(y) do
                        raise WrongSpecError, message: "#{x}: missing deps: #{y}"
                      else
                        true
                      end

                    y ->
                      get_switch_from_spec(specs, y)
                  end
                end)

              if length(found) == 0 do
                raise WrongSpecError, message: "#{x}: missing deps. need any of: #{switch.deps}"
              else
                true
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

  @spec get_default_args([map()], named_args()) :: named_args()
  def get_default_args(spec, parsed_map) do
    List.foldl(spec, parsed_map, fn x, acc ->
      name = long_or_short(x)
      name = String.replace(name, ~r"-", "_") |> String.to_atom()

      cond do
        x.default && not Map.has_key?(parsed_map, name) ->
          Map.put(
            acc,
            name,
            x.default.() |> to_list
          )

        true ->
          acc
      end
    end)
  end

  @spec parse!({str(), str()}, specs()) :: parsed_args()
  def parse!(long_desc, spec) do
    parse(long_desc, spec, System.argv())
  end

  def extract_till_sep(args) do
    sep_pos = Enum.find_index(args, fn x -> x == "--" end) || -1

    cond do
      sep_pos == -1 ->
        {args, []}

      sep_pos ->
        {
          Enum.slice(args, 0, sep_pos),
          Enum.slice(args, sep_pos + 1..length(args))
        }
    end
  end

  def parse(_, _, []) do
    {:error, :no_args}
  end

  def parse(_, [], _) do
    {:error, :no_specs}
  end

  @spec parse({str(), str()}, specs(), argv()) ::
          parsed_args()
          | {:error, :no_args}
          | {:error, :no_specs}

  def parse(long_desc, spec, args) do
    spec = add_defaults(spec)

    cond do
      Enum.find_index(args, fn x -> x == "--help" end) ->
        print_help(long_desc, spec)

      true ->
        {args, tail} = extract_till_sep(args)
        parsed = find_index(args, spec)
        parsed = extract_args(args, parsed, [])
        {parsed, tail1} = check_switch_args(args, parsed, %{}, [])
        parsed_map = get_named_args(parsed, %{})
        parsed_map = post_process_switches(spec, parsed_map)
        parsed_map = get_default_args(spec, parsed_map)
        _ = check_deps(parsed, parsed_map)
        first_pos = List.first(parsed)

        pos_head =
          if first_pos do
            Enum.slice(args, 0, first_pos.pos)
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
                  [] ->
                    switch = get_switch_from_spec(spec, x)

                    if switch.n == 0 do
                      true
                    else
                      []
                    end

                  [item] ->
                    item

                  v ->
                    v
                end

              Map.put(acc, x, out)
            end)
          end

        pos_tail = tail ++ tail1
        pos_args = pos_head ++ pos_tail

        if length(pos_args) > 0 do
          {:ok, {pos_args, parsed_map}}
        else
          {:ok, {[], parsed_map}}
        end
    end
  end

  def test() do
    args =
      Enum.map(
        [
          1,
          2,
          3,
          4,
          "-a",
          "/home/caligian/.bashrc",
          "-b",
          1,
          2,
          3,
          4,
          "-b",
          6,
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
          "4",
          "5"
        ],
        fn x ->
          (is_integer(x) && Integer.to_string(x)) || x
        end
      )

    specs = [
      %{
        name: "a",
        long: "long-name",
        n: 5,
        metavar: :path,
        type: :contents,
        desc: "Hello world, teri maa ka bhosda"
      },
      %{name: "b", n: 4, type: :integer},
      %{name: "c", n: 6, type: :float},
      %{name: "d", default: fn -> :hello end}
    ]

    parse({"hello world!", "some long description"}, specs, args)
  end
end



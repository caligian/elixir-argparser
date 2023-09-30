defmodule Argparser.WrongSpecError do
  @moduledoc """
  Raised when
  - required switch is not passed
  - unallowed characters in switch names
  - post/1 raises an exception while processing parsed args
  - unneeded switches defined in 'without' passed
  - deps of switch are missing
  """

  defexception [:message]
end

defmodule Argparser.WrongNargsError do
  @moduledoc """
  Raised when wrong number of arguments are provided for a switch
  """

  defexception [:message]
end

defmodule Argparser do
  alias Argparser.Help
  alias Argparser.WrongSpecError
  alias Argparser.WrongNargsError
  import Argparser.Utils

  @moduledoc """
  A simple argparser module for elixir.
  """

  @moduledoc since: "1.0.0"

  @typedoc """
  Map representing a commandline option. Everything is optional except any one of name or long (or both) must be passed.

  You can specify number of arguments. This can be either * (greedy-zero-or-mode), + (greedy-one-or-more), ? (zero-or-one) or any integer specifying the number of arguments required. When you use a greedy specifier, it will regard any positional arguments after switches as being extra arguments. Therefore, avoid placing any positional arguments after greedy matchers.

  Specification for 'type':
  :contents -> file contents
  :integer | :int -> integer
  :float -> float
  :regex -> pcre regex
  :dir -> directory path
  :file -> file path

  If 'post' is passed, it will be applied to all the parsed args.
  If 'check' is passed then args will be validated using check/1 :: boolean().
  If 'dup' is true then multiple instances of the switches can be used. All the args will be concatenated into one list otherwise WrongNargsError will be raised.

  You can specify dependencies and non-dependencies. If x in 'deps' is a list then all the keys specified must be presented or else an WrongSpecError will be raised otherwise all. At least one x must be present from the entire spec. If x in 'without' is found in parsed args then WrongSpecError will be raised.

  Rest of the keys are self-explanatory.
  """

  @typedoc "Parse arguments from a list of switch maps"
  @type switch() :: %{
          name: str(),
          long: str(),
          n: str() | integer(),
          post: function(),
          required: boolean(),
          default: any(),
          deps: list(list(str())) | list(str()) | list(),
          desc: str(),
          without: list(str()),
          pos: number(),
          metavar: str(),
          dup: boolean(),
          args: list(str()),
          type:
            :contents
            | :integer
            | :int
            | :float
            | :regex
            | :dir
            | :file
            | :str
            | :string
        }

  @type argv() :: [str()]
  @type specs() :: [switch()]
  @type str() :: String.t()
  @type pos_args() :: [str()]
  @type named_args() :: %{str() => switch()}
  @type parsed_args() :: {pos_args(), named_args()}

  @doc "Get standard input"
  @spec get_stdin() :: [str()] | :error
  def get_stdin(sep \\ ~r"[\n\r]") do
    case IO.read(:stdio, :all) do
      {:error, _} -> :error
      s -> s |> String.split(sep) |> Enum.filter(fn x -> String.length(x) > 0 end)
    end
  end

  @spec read_file(path :: str()) :: list(str()) | :error
  defp read_file(fname) do
    File.stream!(fname)
    |> Enum.chunk_every(2)
    |> List.flatten()
  end

  @spec validate_names(switch()) :: :ok | WrongSpecError
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

    :ok
  end

  defp find_index([], _, _, x, found) do
    found
    |> List.flatten()
    |> Enum.reverse()
    |> Enum.map(&Map.put(x, :pos, &1))
  end

  @spec find_index(argv(), integer(), integer(), switch(), list()) :: switch() | WrongSpecError
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
      false when x.required ->
        raise WrongSpecError, message: "#{name}: must be passed"

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

  @spec find_index([str()], [switch()]) :: [switch()]
  def find_index(args, xs) do
    xs
    |> Enum.map(fn x -> find_index(args, length(args), 0, x, []) end)
    |> List.flatten()
    |> Enum.filter(&Map.has_key?(&1, :pos))
  end

  @spec parse_switch_args(argv(), switch(), number()) :: switch()
  defp parse_switch_args(args, switch, next_switch) when is_number(next_switch) do
    args_len = length(args)
    args_left = args_len - (switch.pos + 1)
    remaining_args = Enum.slice(args, switch.pos + 1, args_left)

    %{switch | args: remaining_args}
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

    %{switch | args: passed_args}
  end

  @spec long_or_short(switch()) :: str()
  defp long_or_short(switch) do
    cond do
      switch.long ->
        switch.long

      true ->
        switch.name
    end
  end

  defp check_switch_args(_args, [], lookup, parsed) do
    {last, args_tail} = hd(parsed)
    parsed = [last | tl(parsed)]

    lookup
    |> Map.keys()
    |> Enum.each(fn x ->
      {passed, n} = lookup[x]

      failed =
        (n == "+" and passed == 0) or (n == "?" and passed > 2) or (is_number(n) and passed != n)

      if failed do
        raise WrongNargsError, message: "#{x}: required #{n}, got #{passed}"
      else
        x
      end
    end)

    {Enum.reverse(parsed), args_tail}
  end

  defp check_switch_args(args, [x], lookup, parsed) do
    n = x.n
    pos = x.pos
    name = long_or_short(x)
    {prev_nargs, _} = lookup[name] || {0, 0}
    current_args = x.args || []
    current_nargs = length(current_args)
    total_nargs = prev_nargs + current_nargs
    args_len = length(args)
    remaining = args_len - (pos + 1)
    lookup = Map.put(lookup, name, {total_nargs, n})

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
              message: "#{name}: expected +, got #{total_nargs}"
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

            n > total_nargs ->
              raise WrongNargsError,
                message: "#{name}: required #{n}, got #{total_nargs}"

            n < prev_nargs ->
              raise WrongNargsError,
                message: "#{name}: required #{n}, got #{current_nargs + prev_nargs}"

            true ->
              req = n - prev_nargs

              cond do
                req > remaining ->
                  raise WrongNargsError,
                    message: "#{name}: required #{n}, got #{total_nargs}"

                true ->
                  Enum.slice(current_args, 0, req)
              end
          end

        true ->
          raise WrongSpecError, message: "#{name}: need any of [0-9]+ or [+*?], got #{n}"
      end

    res = to_list(res)

    args_tail =
      case length(res) do
        0 -> remaining_args
        n when n == remaining -> []
        n -> Enum.slice(remaining_args, n, remaining - n)
      end

    x = {%{x | args: res}, args_tail}

    check_switch_args(args, [], lookup, [x | parsed])
  end

  @spec check_switch_args(argv(), [switch()], switch(), [switch()]) ::
          [switch()] | WrongSpecError | WrongNargsError
  defp check_switch_args(args, [switch | rest], lookup, parsed) do
    name = long_or_short(switch)
    passed = switch.args || []
    {exists, _} = lookup[name] || {0, 0}
    n = exists + length(passed)
    lookup = Map.put(lookup, name, {n, switch.n})

    check_switch_args(args, rest, lookup, [switch | parsed])
  end

  @spec get_switch_from_spec([switch()], str()) :: switch() | false
  defp get_switch_from_spec(spec, name) do
    name = (is_atom(name) && Atom.to_string(name)) || name
    name = String.replace(name, "_", "-")
    switch = Enum.filter(spec, fn x -> x.name == name or x.long == name end)

    Enum.at(switch, 0) || false
  end

  @spec post_process_switches(named_args()) :: [switch()] | WrongSpecError
  defp post_process_switches(parsed_map) do
    List.foldl(Map.keys(parsed_map), parsed_map, fn name, acc ->
      switch = parsed_map[name]
      check = switch.check
      type = switch.type
      args = switch.args
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

              _ when type == :integer or type == :int ->
                Enum.map(args, fn x ->
                  {n, _} = Integer.parse(x)
                  n
                end)

              :regex ->
                Enum.map(args, &Regex.compile!(&1))

              :file ->
                Enum.each(args, fn x ->
                  case File.exists?(x) do
                    false -> raise WrongSpecError, message: "#{name}: invalid file path: #{x}"
                    true -> x
                  end
                end)

              :dir ->
                Enum.each(args, fn x ->
                  case File.dir?(x) do
                    false ->
                      raise WrongSpecError, message: "#{name}: invalid directory path: #{x}"

                    true ->
                      x
                  end
                end)

              :contents ->
                Enum.map(args, fn x ->
                  case File.exists?(x) do
                    false ->
                      raise WrongSpecError, message: "#{name}: invalid file path: #{x}"

                    true ->
                      out = read_file(x)
                      out
                  end
                end)

              :string ->
                args

              :str ->
                args

              _ ->
                raise WrongSpecError, "#{name}: invalid type spec: #{type}"
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
            Map.put(acc, name, %{switch | args: args})
        end
      else
        Map.put(acc, name, %{switch | args: args})
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
    map
  end

  @spec get_named_args([switch()], named_args()) :: named_args()
  defp get_named_args([current | rest], map) do
    name = long_or_short(current)
    prev_x = Map.get(map, name)
    dup = current.dup

    cond do
      prev_x && not dup ->
        raise WrongSpecError, message: "#{current} cannot pass more than instance of this switch"

      true ->
        {_, res} = Map.pop!(current, :name)
        {_, res} = Map.pop!(res, :long)

        args =
          case prev_x do
            nil -> current.args
            prev -> prev.args ++ current.args
          end

        res = %{res | args: args}

        get_named_args(rest, Map.put(map, name, res))
    end
  end

  @spec check_deps(specs(), named_args()) :: nil | WrongSpecError
  defp check_deps(specs, map) do
    List.foldl(Map.keys(map), %{}, fn name, _ ->
      x = map[name]

      case x.without do
        [] ->
          true

        w ->
          found = Enum.filter(w, fn y -> get_switch_from_spec(specs, y) end)

          case length(found) do
            n when n > 0 ->
              raise WrongSpecError, message: "#{name}: unneeded deps found: #{inspect(found)}"

            _ ->
              true
          end
      end

      case x.deps do
        [] ->
          true

        d ->
          found =
            Enum.filter(d, fn m ->
              case m do
                y when is_list(y) ->
                  res = Enum.filter(y, fn z -> get_switch_from_spec(specs, z) end)

                  if length(res) != length(y) do
                    raise WrongSpecError, message: "#{name}: missing deps: #{inspect(y)}"
                  else
                    true
                  end

                y ->
                  get_switch_from_spec(specs, y)
              end
            end)

          if length(found) == 0 do
            raise WrongSpecError,
              message: "#{name}: missing deps. need any of: #{inspect(x.deps)}"
          else
            true
          end
      end
    end)
  end

  def parse_args([]) do
    false
  end

  @spec to_list(any()) :: [any()] | any()
  defp to_list(x) do
    if not is_list(x) do
      [x]
    else
      x
    end
  end

  @spec extract_till_sep(args()) :: tuple(list(str()), list(str()))
  def extract_till_sep(args) do
    sep_pos = Enum.find_index(args, fn x -> x == "--" end) || -1

    cond do
      sep_pos == -1 ->
        {args, []}

      sep_pos ->
        {
          Enum.slice(args, 0, sep_pos),
          Enum.slice(args, (sep_pos + 1)..length(args))
        }
    end
  end

  def parse(_, _, []) do
    raise WrongSpecError, message: "no argv provided"
  end

  def parse(_, [], _) do
    raise WrongSpecError, message: "no specs provided"
  end

  @spec parse({str(), str()}, specs(), argv()) ::
          parsed_args()
          | WrongSpecError
          | WrongNargsError
          | {:error, :no_args}
          | {:error, :no_specs}
  def parse(desc, spec, args) do
    cond do
      Enum.find_index(args, fn x -> x == "--help" or x == "-h" end) ->
        Help.help(desc, spec)

      true ->
        spec = add_defaults(spec)
        {args, tail} = extract_till_sep(args)
        parsed = find_index(args, spec)
        parsed = extract_args(args, parsed, [])
        {parsed, tail1} = check_switch_args(args, parsed, %{}, [])
        parsed_map = get_named_args(parsed, %{})
        parsed_map = post_process_switches(parsed_map)
        check_deps(parsed, parsed_map)
        first_pos = List.first(parsed)

        pos_head =
          if first_pos do
            Enum.slice(args, 0, first_pos.pos)
          else
            []
          end

        parsed_map = (map_size(parsed_map) > 0 && parsed_map) || false
        pos_tail = tail ++ tail1
        pos_args = pos_head ++ pos_tail

        if length(pos_args) > 0 do
          {:ok, {pos_args, parsed_map}}
        else
          {:ok, {[], parsed_map}}
        end
    end
  end

  @spec parse!({str(), str()}, specs()) ::
          parsed_args()
          | WrongSpecError
          | WrongNargsError
          | {:error, :no_args}
          | {:error, :no_specs}

  def parse!(desc, spec) do
    parse(desc, spec, System.argv())
  end
end



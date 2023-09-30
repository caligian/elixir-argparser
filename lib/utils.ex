defmodule Argparser.Utils do
  def add_defaults(x) when is_map(x) do
    x = Map.merge(get_default(), x)

    if not x[:metavar] do
      Map.put(x, :metavar, String.upcase(x[:long] || x[:name]))
    else
      x
    end
  end

  def add_defaults(xs) when is_list(xs) do
    Enum.map(xs, &add_defaults/1)
  end

  def get_default() do
    %{
      name: false,
      n: 0,
      metavar: false,
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
end

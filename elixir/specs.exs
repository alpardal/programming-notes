defmodule SomeBehaviour do
  @callback some_callback() :: boolean()
end

defmodule TypeSpecsCheatSheet do
  # creating a custom type:
  @type word() :: String.t()

  # simple spec:
  @spec long_word?(word()) :: boolean()
  def long_word?(word) when is_binary(word) do
    String.length(word) > 8
  end

  # no args, pid type:
  @spec my_pid() :: pid()
  def my_pid do
    self()
  end

  # implementing a behavior:
  @behaviour SomeBehaviour
  def some_callback(), do: true

  # using guards:
  @spec with_guards(arg) :: [arg] when arg: atom
  def with_guards(val) when is_atom(val) do
    [val]
  end

  # using maps:
  @type person() :: %{name: String.t()}

  @spec john() :: person()
  def john() do
    %{name: "john"}
  end

  @spec print_name(person()) :: none()
  def print_name(person) do
    IO.puts person.name
  end
end

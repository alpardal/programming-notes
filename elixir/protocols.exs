defmodule Size do
  defprotocol Sized do
    @fallback_to_any true
    def size_of(data)
  end

  def of(data), do: Sized.size_of(data)

  def double(data), do: of(data) * 2
end

defimpl Size.Sized, for: Any do
  def size_of(_), do: 0
end

defimpl Size.Sized, for: Tuple do
  def size_of(tuple), do: tuple_size(tuple)
end

defmodule Person do
  defstruct [:name, :age]
end

defimpl Size.Sized, for: Person do
  def size_of(person), do: person.age
end

defmodule User do
  # `fallback_to_any` not needed w/ use of derive:
  @derive [Size.Sized]
  defstruct [:email]
end

defmodule Tests do
  def run do
    IO.puts("tuple size: #{Size.of({:ok, 1})}")
    IO.puts("person size: #{Size.of(%Person{name: "John", age: 41})}")
    IO.puts("derived fallback: #{Size.of(%User{email: "bla@example.com"})}")
    IO.puts("`Any` fallback: #{Size.of(:ok)}")
    IO.puts("double-size: #{Size.double({:ok, 1})}")
    IO.puts("")

    IO.puts("find out the implementation for a value:")
    IO.puts("  #{Size.Sized.impl_for({}) |> inspect}")
    IO.puts("  #{Size.Sized.impl_for(%{}) |> inspect}")
    IO.puts("  #{Enumerable.impl_for(%{}) |> inspect}")
    IO.puts("  #{Enumerable.impl_for({}) |> inspect}")
    IO.puts("")

    # for compiled code:
    IO.puts("finding all implementations of a given protocol:")
    IO.puts("  #{Size.Sized.__protocol__(:impls) |> inspect}")
  end
end

Tests.run()

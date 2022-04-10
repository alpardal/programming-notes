defmodule User do
  @enforce_keys [:name]
  defstruct [:name, :age]
  # same as:
  # defstruct name: nil, age: nil

  def run do
    IO.inspect %User{name: "John"}
    paul = %User{name: "Paul", age: 40}
    IO.inspect paul
    IO.inspect %{paul | age: 38}
    %User{age: age} = paul
    IO.inspect age
    # IO.inspect %User{invalid_field: "value"} # won't compile
    # IO.inspect %{paul | invalid_field: "value"} # won't compile either
    # IO.inspect %User{} # won't compile due to `@enforce_keys`
  end
end

User.run

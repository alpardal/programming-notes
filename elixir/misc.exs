defmodule Misc do
  def getting_value_info do
    val = %{one: 1, two: 2}
    IO.puts "val: #{inspect val}"
    IO.puts "info: #{val |> IEx.Info.info() |> inspect}"
  end

  def getting_module_functions do
    IO.inspect __MODULE__.__info__(:functions)
  end

  def run do
    getting_value_info()
    getting_module_functions()
  end
end

Misc.run

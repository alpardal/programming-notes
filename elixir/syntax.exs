defmodule Syntax do
  def anonymous_functions do
    add = fn a, b -> a + b end
    IO.puts(is_function(add))
    # called with a '.' between name and '('
    IO.puts(add.(1, 2))

    # shortcut syntax
    add2 = &(&1 + &2)
    IO.puts(add2.(1, 2))

    # partial application
    square = &:math.pow(&1, 2)
    IO.puts(square.(2))

    # string and string-like captures:
    prefixed = &"Mr. #{&1}"
    IO.puts(prefixed.("Andrews"))
    comment_matcher = &~r/^\s*#{&1}/
    IO.inspect("# elixir comment" =~ comment_matcher.("#"))
    in_tuple = &{&1, 2}
    IO.inspect(in_tuple.(1))
    in_list = &[1, &1]
    IO.inspect(in_list.(2))
  end

  def comprehensions do
    # see https://hexdocs.pm/elixir/Kernel.SpecialForms.html#for/1
    data = [strings: ["one", "two"], ints: [1, 2, 3], strings: ["three"]]

    strings =
      for {:strings, vals} <- data,
          val <- vals do
        val
      end

    IO.inspect(strings)
  end

  def tuples do
    tuple = {:status, :ok}
    IO.inspect(tuple)
  end

  def concatenation do
    # strings
    IO.inspect("bla" <> " " <> "ble")
    # lists
    list = [1, 2, 3] ++ [4, 5, 6]
    IO.inspect(list)
  end

  def bools do
    IO.inspect(true or false)
    IO.inspect(true and not false)
    # || and && accept any value
    IO.inspect(0 || nil)
    # only `nil` and `false` are falsy
    IO.inspect(true && nil)
    IO.inspect(!!0)
  end

  def destructuring do
    {status, _value} = {:ok, 1}
    IO.inspect(status)

    [_x | xs] = [1, 2, 3]
    IO.inspect(xs)

    %{a: a} = %{a: "value of :a"}
    IO.inspect(a)
  end

  def pattern_matching do
    res =
      case [1, 2] do
        [_x | xs] -> xs
        [] -> "empty"
        _ -> "unreachable"
      end

    IO.inspect(res)

    res2 =
      case :something do
        ^res -> "won't match"
        _ -> "different from `res`"
      end

    IO.inspect(res2)

    # with guards:
    res3 =
      case res do
        [x] when x > 2 -> "won't match"
        _ -> "less than two"
      end

    IO.inspect(res3)

    # string/binary prefixes:
    res4 =
      case "video:1" do
        "video:" <> id -> {:ok, id}
        _ -> {:error, "not a video tag"}
      end

    IO.inspect(res4)
  end

  def conds do
    res =
      cond do
        2 + 2 == 5 ->
          "This is never true"

        2 * 2 == 3 ->
          "Nor this"

        true ->
          "This is always true (equivalent to else)"
      end

    IO.inspect(res)
  end

  def conditionals do
    if true do
      IO.inspect("true indeed")
    end

    # using keyword list:
    val = if true, do: :is_true, else: :not_true
    IO.inspect(val)
  end

  def unicode do
    IO.inspect(String.codepoints("日本"))
  end

  def maps do
    map = %{a: 1, b: 2}
    IO.inspect(%{map | b: 3})
    IO.inspect(map.a)
  end

  def function_definitions do
    show_sign(0)
    show_sign(2)
    show_sign(-2)
  end

  defp show_sign(0), do: IO.puts("value is zero.")
  defp show_sign(x) when x > 0, do: IO.puts("value is positive")
  defp show_sign(_), do: IO.puts("value is negative.")

  def function_heads(arg \\ "a default value")

  def function_heads(arg) do
    IO.puts("this function hash a head that sets a default value: #{arg}")
  end

  def function_references do
    f = &show_sign/1
    f.(0)
  end

  def default_args do
    default_args_impl()
    default_args_impl(4)
  end

  defp default_args_impl(arg \\ 3) do
    IO.inspect(arg)
  end

  # recursion:
  def fact do
    IO.puts(fact_impl(5))
  end

  defp fact_impl(0), do: 1
  defp fact_impl(x), do: x * fact_impl(x - 1)

  def with_clauses do
    IO.inspect(area(%{width: 10, height: 15}))
    IO.inspect(area(%{invalid: "map"}))
  end

  defp area(attrs) do
    # optional else clause
    with {:ok, width} <- Map.fetch(attrs, :width),
         {:ok, height} <- Map.fetch(attrs, :height) do
      {:ok, width * height}
    else
      :error -> {:error, "Invalid attrs"}
    end
  end

  def importing do
    import IO, only: [puts: 1]
    puts("imported!")
  end

  def string_formatting do
    # uses erlang libs:
    # ~sprintf
    str = :io_lib.format("~.4f", [:math.pi()])
    IO.puts(str)
    # ~printf
    :io.format("~.10f~n", [:math.pi()])
  end

  def errors do
    # type can one of: :error, :exit or :throw
    try do
      raise "a runtime error"
    catch
      type, value -> IO.puts("#{inspect(type)} - #{inspect(value)}")
    after
      IO.puts("error handled.")
    end
  end

  def date_literals do
    IO.inspect(~D[2019-01-30])
  end

  def run_all do
    m = __MODULE__
    run(&m.anonymous_functions/0)
    run(&m.comprehensions/0)
    run(&m.tuples/0)
    run(&m.concatenation/0)
    run(&m.bools/0)
    run(&m.destructuring/0)
    run(&m.pattern_matching/0)
    run(&m.conds/0)
    run(&m.conditionals/0)
    run(&m.maps/0)
    run(&m.function_definitions/0)
    run(&m.function_heads/0)
    run(&m.function_references/0)
    run(&m.default_args/0)
    run(&m.fact/0)
    run(&m.with_clauses/0)
    run(&m.importing/0)
    run(&m.string_formatting/0)
    run(&m.errors/0)
    run(&m.date_literals/0)
  end

  defp run(fun) do
    name = Keyword.get(Function.info(fun), :name) |> to_string
    IO.puts("- #{name}:")
    fun.()
    IO.puts("----------------------------")
  end
end

Syntax.run_all()

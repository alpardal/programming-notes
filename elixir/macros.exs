defmodule MacroTests do
  # see https://hexdocs.pm/elixir/Kernel.SpecialForms.html#quote/2-quote-and-macros
  def ast_representation do
    asts = [
      {:x, [], Elixir},                               # variable
      :bla,                                           # atom literal
      "bla",                                          # string literal
      {:bla},                                         # tuple literal
      [1, 2],                                         # lists, nums
      {:sum, [], [1, 2]},                             # simple function call
      {{:., [], [String, :upcase]}, [], ["bla"]},     # module function call
      {{:., [], [:math, :pi]}, [no_parens: true], []} # ditto w/out parens
      # see https://hexdocs.pm/elixir/Kernel.SpecialForms.html#./2-quoted-expression
    ]
    asts
    |> Enum.map(&Macro.to_string/1)
  end

  def ast_evaluation do
    [
      unquote([1, 2]),
      unquote do
        {{:., [], [String, :upcase]}, [], ["bla"]}
      end,
    ]
  end

  def splicing do
    [
      Macro.to_string(quote do: [1, 2, unquote([3, 4]), 5]),
      Macro.to_string(quote do: [1, 2, unquote_splicing([3, 4]), 5])
    ]
  end

  def expanding do
    # won't work running as script/without Mix:
    # [
    #   Macro.expand_once(quote do: [1, 2, 3], __CALLER__),
    #   Macro.expand(quote do: [1, 2, 3], __CALLER__)
    # ]
  end
end

IO.inspect MacroTests.ast_representation
IO.inspect MacroTests.ast_evaluation
IO.inspect MacroTests.splicing
IO.inspect Macro.Env.new
# IO.inspect MacroTests.expanding

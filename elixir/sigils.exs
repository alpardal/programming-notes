
IO.puts "Regexes:"

regex = ~r/(\w+)-(\d+)/
text = "bla-1238"
IO.inspect text =~ regex, label: "  is a match"
IO.inspect Regex.run(regex, text), label: "  captures"

# alternative sigil delimiters:
alternatives = [
  ~r|hello|,
  ~r"hello",
  ~r'hello',
  ~r(hello),
  ~r[hello],
  ~r{hello},
  ~r<hello>
]
IO.inspect Enum.all?(alternatives, &(&1 === ~r/hello/)),
           label: "  same regex"

IO.puts ""
IO.puts "Strings:"
IO.inspect ~s(A #{'string'}),
           label: "  lower-case sigil, supports escaping/interpolation"
IO.inspect ~S(won't be interpolated), label: "  upper-case sigil"

IO.inspect ~c(a char list), label: "  char list"
IO.inspect ~w(a list of words), label: "  word list"
IO.inspect ~w(a list of atoms)a, label: "  word list"

IO.puts ~s"""
  This is a nice
  heredoc string.
"""

IO.puts "Dates/times:"

IO.inspect ~D[2019-08-19], label: "  a date"
IO.inspect ~N[2020-02-15 15:31:00], label: "  a naive datetime"
IO.inspect ~T[15:31:00], label: "  a time"
IO.puts ""


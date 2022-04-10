
data = for n <- [1,2,3], do: n * n
IO.inspect data

# any enumerable can be used as generator:
IO.inspect for n <- 1..4, do: 2 * n

# pattern matching:
values = [ok: 1, error: 2, ok: 3]
IO.inspect for {:ok, val} <- values, do: val

# filters:
even? = &(rem(&1, 2) == 0)
IO.inspect for n <- 1..10, even?.(n), do: n

# multiple generators:
IO.inspect for n <- 1..2, m <- 1..2, do: [n, m]

# multiple filters:
triple? = &(rem(&1, 3) == 0)
IO.inspect for n <- 1..100, even?.(n), triple?.(n), do: n

# intermediary bindings:
IO.inspect for n <- 1..5, double = 2 * n, do: -double

# w/ bitstrings:
pixels = <<213, 45, 132, 64, 76, 32, 76, 0, 0, 234, 32, 15>>
IO.inspect for <<r::8, g::8, b::8 <- pixels>>, do: {r, g, b}

# using `into`:
data = %{a: 1, b: 2, c: 3}
IO.inspect for {k, v} <- data, into: %{}, do: {k, v * 2}

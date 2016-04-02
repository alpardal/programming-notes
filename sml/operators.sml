
infix |>
fun x |> f = f x
(* same as: *)
fun op |>(x, f) = f x

fun sqrt_of_abs i = i |> abs |> Real.fromInt |> Math.sqrt

val sqrt = sqrt_of_abs ~10

fun fold(f, e, xs) =
  case xs of
       [] => e
     | x::xs' => f(fold(f, e, xs'), x)

val sum = fold(op +, 0, [1,2,3,4,5])

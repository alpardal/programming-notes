let put_int m = print_int m ; print_endline ""

let put_float m = Printf.printf "%F\n" m

let cat a b = a ^ "; " ^ b

let string_of_int_list is =
  let vals = List.map string_of_int is in
  "[" ^ List.fold_left cat "" vals ^ "]"

let put_ints ls = print_endline (string_of_int_list ls)

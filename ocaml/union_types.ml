(* or 'variant types' *)

type number = Int of int | Float of float | Error

let print_number n =
  match n with
  | Int v -> print_int v ; print_endline ""
  | Float f -> print_float f ; print_endline ""
  | Error -> print_string "Error" ; print_endline ""

;;
print_number (Float 2.)

;;
print_number Error

(* option, already exists in OCaml: *)
(* type 'a option = Some of 'a | None *)

type 'a btree = Empty | Noe of 'a * 'a btree * 'a btree

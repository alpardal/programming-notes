let name_of_binary_digit digit =
  try List.assoc digit [(0, "zero"); (1, "one")] with Not_found ->
    "not a binary digit"

;;
print_endline (name_of_binary_digit 0)

;;
print_endline (name_of_binary_digit 2)

(* let flat_assoc_opt x l = *)
(*   match List.assoc x l with *)
(*   | None | (exception Not_found) -> None *)
(*   | Some _ as v -> v *)

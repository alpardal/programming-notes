(* #use "utils.ml" *)

open Printf

(* string delimiters: *)

let msg = {name|A string with an | arbitrary delimiter|name}

;;
print_endline msg

(* recursive functions: *)

let rec fact x = if x <= 2 then x else x * fact (x - 1)

;;
printf "fact of 5: %d\n" (fact 5)

(* anonymous functions: *)

;;
printf "IIFE: %d\n" ((function x -> x * 2) 5)

let rec last = function [] -> None | [x] -> Some x | _ :: t -> last t

(* `let in`: *)

;;
let l = last [1; 2; 3] in
match l with
| None -> print_endline "empty"
| Some v -> printf "last list item: %d\n" v

(* turning operators into functions: *)

;;
printf "sum of list: %d\n" (List.fold_left ( + ) 0 [1; 2; 3])

(* insertion sort - only working on repl *)
(* let rect sort lst = *)
(*   match lst with *)
(*   [] -> [] *)
(*   | head :: tail -> insert head (sort  tail) *)
(* and insert el lst = *)
(*   match lst with *)
(*   [] -> [el] *)
(*   | head :: tail -> if el < head el :: lst else head :: insert el tail *)
(* ;; *)

#use "utils.ml"

type expression =
  | Const of float
  | Var of string
  | Sum of expression * expression
  | Diff of expression * expression
  | Prod of expression * expression
  | Quot of expression * expression

exception Unbound_variable of string

let rec eval env exp =
  match exp with
  | Const c -> c
  | Var v -> (
    try List.assoc v env with Not_found -> raise (Unbound_variable v) )
  | Sum (f, g) -> eval env f +. eval env g
  | Diff (f, g) -> eval env f -. eval env g
  | Prod (f, g) -> eval env f *. eval env g
  | Quot (f, g) -> eval env f /. eval env g

(* x / (y + 3) *)
let exp = Quot (Var "x", Sum (Var "y", Const 3.))

(* x = 3; y = 2 *)
let res = eval [("x", 3.); ("y", 2.)] exp

;;
put_float res
;;

let rec deriv exp dv =
  match exp with
  | Const c -> Const 0.
  | Var v -> if v = dv then Const 1. else Const 0.
  | Sum (f, g) -> Sum (deriv f dv, deriv g dv)
  | Diff (f, g) -> Diff (deriv f dv, deriv g dv)
  | Prod (f, g) -> Sum (Prod (f, deriv g dv), Prod (deriv f dv, g))
  | Quot (f, g) ->
      Quot (Diff (Prod (deriv f dv, g), Prod (f, deriv g dv)), Prod (g, g))

;;
let print_exp exp =
  let open_paren prec op_prec =
    if prec > op_prec then print_string "(" in
  let close_paren prec op_prec =
    if prec > op_prec then print_string ")" in
  let rec print prec exp =
    match exp with
      Const c -> print_float c
    | Var v -> print_string v
    | Sum(f, g) ->
        open_paren prec 0;
        print 0 f; print_string " + "; print 0 g;
        close_paren prec 0
    | Diff(f, g) ->
        open_paren prec 0;
        print 0 f; print_string " - "; print 1 g;
        close_paren prec 0;
    | Prod(f, g) ->
        open_paren prec 2;
        print 2 f; print_string " * "; print 2 g;
        close_paren prec 2
    | Quot(f, g) ->
        open_paren prec 2;
        print 2 f; print_string " / "; print 3 g;
        close_paren prec 2
  in print 0 exp; print_endline "";;

let exp2 = Prod (Var "x", Var "x") (* x^2 *)

let dexp2 = deriv exp2 "x"     (* 2*x *)
;;
print_exp exp2
;;
print_exp dexp2
;;

let env = [("x", 3.)]

let env_eval = eval env
;;
put_float (env_eval exp2)
;;
put_float (env_eval dexp2)

(* named ('labeled') args: *)
(* call order doesn't matter anymore: *)

open Printf

let div ~num ~denom = num /. denom

;;
printf "%F\n" (div 1. 2.)

;;
printf "%F\n" (div ~denom:2. ~num:1.)

(* labels can also be used for optional arguments: *)

let div ?(denom = 2.) num = num /. denom

;;
printf "%F\n" (div 1.)

;;
printf "%F\n" (div 1. ~denom:3.)

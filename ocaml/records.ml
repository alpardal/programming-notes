#use "utils.ml"

type ratio = {num:int; denom:int}

(* constructor: *)
let ratio num denom = {num; denom}
;;
let get_num {num} = num
;;

let add_ratio r1 r2 =
  {num= (r1.num * r2.denom) + (r2.num * r1.denom); denom= r1.denom * r2.denom}

let integer_part r = match r with {num; denom} -> num / denom

;;
put_int (integer_part {num= 12; denom= 10})

;;
(* or using the constuctor: *)
put_int (integer_part (ratio 12 10))

;;

(* updating: *)

let r = ratio 3 2;;

put_int (get_num {r with num = 2 * r.num})
;;

put_ints [1;2;3]

(* Arrays are mutable: *)

let add_vect v1 v2 =
  let len = min (Array.length v1) (Array.length v2) in
  let res = Array.make len 0.0 in
  for i = 0 to len - 1 do
    res.(i) <- v1.(i) +. v2.(i)
  done ;
  res

;;
add_vect [|1.0; 2.0|] [|3.0; 4.0|]

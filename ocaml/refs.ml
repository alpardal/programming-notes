(*
 * j is a ref
 *
 *   - `!j` de-refs its value
 *   - `:=` assigns a new value
 *
 * references are simple mutable records:
 *
 *   type 'a ref = { mutable contents: 'a' };;
 *
 *)

let insertion_sort a =
  for i = 1 to Array.length a - 1 do
    let val_i = a.(1) in
    let j = ref i in
    while !j > 0 && val_i < a.(!j - 1) do
      a.(!j) <- a.(!j - 1) ;
      j := !j - 1
    done ;
    a.(!j) <- val_i
  done

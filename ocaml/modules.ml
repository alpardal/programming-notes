(* should go into `PrioQueue.mli`, without the 'module ... end' part *)
module type PRIOQUEUE = sig
  type priority = int

  type 'a queue

  val empty : 'a queue

  val insert : 'a queue -> int -> 'a -> 'a queue

  val extract : 'a queue -> int * 'a * 'a queue

  exception Queue_is_empty
end

(* should go into `PrioQueue.ml`, without the 'module ... end' part *)
module PrioQueue : PRIOQUEUE = struct
  type priority = int

  type 'a queue = Empty | Node of priority * 'a * 'a queue * 'a queue

  let empty = Empty

  let rec insert queue prio elt =
    match queue with
    | Empty -> Node (prio, elt, Empty, Empty)
    | Node (p, e, left, right) ->
        if prio <= p then Node (prio, elt, insert right p e, left)
        else Node (p, e, insert right prio elt, left)

  exception Queue_is_empty

  let rec remove_top = function
    | Empty -> raise Queue_is_empty
    | Node (prio, elt, left, Empty) -> left
    | Node (prio, elt, Empty, right) -> right
    | Node
        ( prio
        , elt
        , (Node (lprio, lelt, _, _) as left)
        , (Node (rprio, relt, _, _) as right) ) ->
        if lprio <= rprio then Node (lprio, lelt, remove_top left, right)
        else Node (rprio, relt, left, remove_top right)

  let extract = function
    | Empty -> raise Queue_is_empty
    | Node (prio, elt, _, _) as queue -> (prio, elt, remove_top queue)
end

let queue = PrioQueue.insert PrioQueue.empty 2 "hello"

let queue = PrioQueue.insert queue 0 "even higher"

let queue = PrioQueue.insert queue 1 "higher"

let _, v, queue = PrioQueue.extract queue

;;
print_endline v

open PrioQueue

(* all module's methods become visible *)
let _, v, _ = extract queue

(* or: *)
let _, v, _ =
  let open PrioQueue in
  extract queue

(* or: *)
let _, v, _ = PrioQueue.(extract queue)

;;
print_endline v

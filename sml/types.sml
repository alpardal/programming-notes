
datatype mytype = TwoInts of int*int
                | Str of string
                | Pizza

fun f(x : mytype) =
  case x of
       Pizza => 3
     | Str _ => 8
     | TwoInts(i1, i2) => i1+i2


(* type synonyms: *)
type student_id = int
type student = {id : student_id, name : string}

(* ex: *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank


(* polymorphic datatypes *)

datatype 'a maybe = Nothing
                  | Just of 'a (* same actual definition of option *)

(* multiple type parameters: *)
datatype ('a, 'b) one_or_two = Two of 'a * 'a
                             | One of 'b


(* equality types: *)
fun are_equal(x : ''a, y : ''a) = x = y
(* the double ' means any type which can be compared with `=` *)
(* -> most types, but not all: e.g., functions, reals: *)
(* `are_equal(2.0, 1.0)` doesn't compile *)


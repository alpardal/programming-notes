val x = 1
val y = ~2
val z = x + y

fun pow(x: int, y: int) =
  if y = 0
  then 1
  else x * pow(x, y-1)

(* int*int -> int*int *)
fun div_mod(x : int, y : int) =
  (x div y, x mod y)

(* tuples *)
fun first(pair : ('a * 'a)) = #1 pair
fun second(pair : ('a * 'a)) = #2 pair

(* lists *)
val empty_list = []
val empty_too = nil (* same thing *)
val is_empty = null [] (* true *)
val also_empty = null nil (* true *)

val a_list = 1::nil (* [1] *)
val appended = [1,2,3] @ [4,5,6] (* [1,2,3,4,5,6] *)
val head = hd appended
val tail = tl appended

(* if-then-else: *)
fun range(from : int, to : int) =
  if from > to
  then []
  else from :: range(from + 1, to)

(* records: *)
val person = {name = "John", age = 10}
val name = #name person
(* tuples are records: *)
val data = {2="boing", 1="bla"}
val first = #1 data
val are_equal = data = ("bla", "boing") (* true *)
val not_a_tuple = {1 = 1, 3 = 3} (* missing index 2 *)

(* pattern matching: *)
fun size(lst : 'a list) =
  case lst of
       [] => 0
     | _::xs => 1 + size(xs)
 (* or: *)
fun size2 [] = 0
  | size2 (_::xs) = 1 + size2(xs)
(* functions always take only one arg and pattern match against it *)
fun get_name {name: string, age: int} = name
fun get_name2 {name, age} = name (* same as above *)
type person = {name: string, age: int}
fun get_name3({name,...} : person) = name
val first::rest = [1,2,3]

(* HOFs: *)
fun apply(f, arg) = f arg

fun fold(xs : 'a list, e : 'b, f : 'a*'b -> 'b) =
  case xs of
       [] => e
     | x::xs' => f(x, fold(xs', e, f))

(* anonymous functions: *)
val sum = fold([1,2,3], 0, fn(x,y) => x+y)

(* currying and partial application: *)
fun add x y = x+y
val add2 = add 2
val five = add2 3
fun curry f x y = f(x,y)
val add' = curry op + (* same as `add` above *)
val inc = add' 1

(* function composition *)
val double = curry op * 2
val inc_and_double = double o inc

(* built-in option monad: *)
fun max(xs : int list) =
  if null xs
  then NONE
  else
    let val max_rest = max(tl xs)
    in if isSome(max_rest) andalso valOf(max_rest) > hd(xs)
       then max_rest
       else SOME(hd xs)
    end

exception Oops
exception OopsWithMessage of string
fun raise_exn(e : exn) = raise e

val res = raise_exn(Oops) handle Oops => "handled!"

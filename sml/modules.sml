signature DATE_ =
sig

  type date (* abstract type: Date function not visible outside module *)
  exception InvalidDate

  val create : int*int*int -> date
  val year : date -> int
  val month : date -> int
  val day : date -> int
  val toString : date -> string
end

structure Date :> DATE_ =
struct

  type year = int
  type month = int
  type day = int

  datatype date = Date of year*month*day
  exception InvalidDate

  (* private module function, not exported in signature *)
  fun valid_date(Date(year, month, day)) = true

  fun create(year, month, day) =
    let val date = Date(year, month, day) in
      if valid_date date
      then date
      else raise InvalidDate
    end

  fun year(Date(year, _, _)) = year

  fun month(Date(_, month, _)) = month

  fun day(Date(_, _, day)) = day

  fun toString(Date(year, month, day)) =
    Int.toString(year) ^ "/" ^ Int.toString(month) ^ "/" ^
    Int.toString(day)

end

val d = Date.create(2014, 10, 2);

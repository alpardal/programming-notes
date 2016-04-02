
// x and y are private and immutable:
class Point(x: Int, y: Int)

// x and y are public and immutable
class Point(val x: Int, val y: Int)
// equivalent to:
class Point(_x: Int, _y: Int) {
  val x = _x
  val y = _y
}

// public and mutable
class Point(var x: Int, var y: Int)

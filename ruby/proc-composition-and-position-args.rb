
square = ->(x) { x ** 2 }
inc = -> { _1 + 1 }
add = -> { _1 + _2 }.curry

arg = 2
puts "(arg is #{arg})"
puts "inc then square: #{(inc >> square).call(arg)}"
puts "square then inc: #{(square >> inc).call(arg)}"
puts "  right-to-left: #{(inc << square).call(arg)}"

puts "positional args: #{add[1, 2]}"
puts "curried: #{add[1][2]}"

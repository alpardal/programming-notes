
def hash_test(data)
  alice = 'Alice'

  case data
    in {name:  'John'}
    puts 'is John'
    in name: 'Alice', children: []         # braces are not needed
    puts 'alice, no children'
    in name: ^alice, children: [_, :bob]   # using bound variables
    puts 'alice, second child is bob'
    in name: the_name                      # re-binding
    puts "somebody else: #{the_name}"
    in children:                           # same as `children: children`
      puts "no name, w/ children: #{children}"
  else
    puts 'no name, no children'
  end
end

hash_test({name: 'John', children: [:alice, :bob]})
hash_test({name: 'Alice', children: []})
hash_test({name: 'Alice', children: [:charlie, :bob]})
hash_test({name: 'Peter'})
hash_test({children: ['Peter']})
hash_test({other: 'value'})
puts

def array_test(n, d)
  div = -> { d.zero?? [:error, 'divide by zero'] : [:ok, n / d] }

  case div.call
    in [:ok, 1] | [:ok, 1.0]                # alternation
      puts 'result is one'
    in [:ok, val] if val == 0               # guard clause
    puts 'result is zero'
    in [:ok, Integer => v]                  # matching by class
      puts "non-zero int: #{v}"
    in [:ok, v]
      puts "other non-zero: #{v}"
    in [:error, reason]
      puts "error: #{reason} (#{n} / #{d})"
  end
end

array_test(10, 10)
array_test(0, 2)
array_test(10, 2)
array_test(10, 2.0)
array_test(10, 0.0)
array_test(10, 0)

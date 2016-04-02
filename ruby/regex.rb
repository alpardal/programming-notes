
# named captures: (?<name>)
'some 123 digits'.match(/(?<number>\d+)/)['number'] # => "123"

# non-capturing group: (?:)
'bla ble bli'.match(/\s(bl[aei])\s/).captures # => ["ble"]
'bla ble bli'.match(/\s(?:bl[aei])\s/).captures # => []

# look-ahead - positive: (?=) negative: (?!)
'1+2=3'.scan(/\d(?==)/) # => ["2"]
'1+2=3'.scan(/\d(?!=)/) # => ["1", "3"]

# look-behind - positive: (?<=) negative: (?<!)
'1+2=3'.scan(/(?<==)\d/) # => ["3"]
'1+2=3'.scan(/(?<!=)\d/) # => ["1", "2"]


-- literal
john = { name: "John", age: 40 }

-- updating:
paul = john { name = "Paul", age = john.age - 10 }


class Nameable n where
  name :: n -> String

instance Nameable String where
  name = id


-- with multiple functions and defaults:
class Eq a where
  (==), (\=) :: a -> a -> Bool
  x /= y = not (x == y)
  x == y = not (x /= y)

-- with superclass:
class Eq a => Ord a where -- Ord is subclass of Eq
  compare :: a -> a -> Ordering
  -- ...

-- instance with qualified type:
instance Eq a => Eq [a] where
  []     == []     = True
  (x:xs) == (y:ys) = x == y && xs == ys
  _      == _      = False

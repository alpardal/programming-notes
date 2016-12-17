
-- Functor:

  fmap id = id
  fmap (g . f) = fmap g . fmap f


-- Applicative Functor:

  pure id <*> v               =  v                  -- identity
  pure f <*> pure x           =  pure (f x)         -- Homomorphism
  u <*> pure y                =  pure ($ y) <*> u   -- Interchange
  pure (.) <*> u <*> v <*> w  =  u <*> (v <*> w)    -- Composition
  -- bonus (follows from above):
    fmap f x  =  pure f <*> x  -- fmap



-- Monad:

  m >>= return     =  m                        -- right unit
  return x >>= f   =  f x                      -- left unit
  (m >>= f) >>= g  =  m >>= (\x -> f x >>= g)  -- associativity

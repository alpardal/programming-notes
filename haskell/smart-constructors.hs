{-# LANGUAGE ViewPatterns #-}

module SmartConstructors (Range(), range, RangeObs(..), r) where

data Range = Range Integer Integer
data RangeObs = R Integer Integer

range :: Integer -> Integer -> Range
range a b = if a <= b then Range a b else error "a must be <= b"

r :: Range -> RangeObs
r (Range a b) = R a b

-- sample usage, using ViewPatterns:
--
--   case rng of
--      (r -> R a b) -> "[" ++ show a ++ "," ++ show b ++ "]

{-# LANGUAGE ViewPatterns #-}
-- https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns

import Data.Char (toLower)

data Person = Person { firstName :: String
                     , lastName :: String }

-- allows pattern matching against function result:

isJohn :: Person -> Bool
isJohn ((map toLower) . firstName -> "john") = True
isJohn _                                     = False

main :: IO ()
main = do
  putStrLn $ show $ isJohn (Person "JoHn" "Doe")

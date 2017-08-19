{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Monoid

data Person = Person { firstName :: String
                     , lastName :: String } deriving Show

greet :: Person -> String
greet (Person { firstName = name }) = "Hi, " <> name <> "!"

-- or, using 'NamedFieldPuns':
punnedGreet :: Person -> String
punnedGreet (Person { firstName }) = "Hi, " <> firstName <> "!"

-- or, with 'RecordWildCards':
wildGreet :: Person -> String
wildGreet (Person { .. }) = "Hi, " <> firstName <> "!"

-- updating field:
blankFirstName :: Person -> Person
blankFirstName p = p { firstName = "" }

main :: IO ()
main = do
  putStrLn $ show (Person "John" "Smith")
  putStrLn $ firstName (Person { firstName = "John"
                               , lastName = "Johnson" })


{-# LANGUAGE NamedFieldPuns,
             LambdaCase      #-}

import Data.List (dropWhile)


skipUntilJust = dropWhile (\case Nothing -> True; _ -> False)


main :: IO ()
main = do
  putStrLn $ show $ skipUntilJust [Nothing, Just 1]

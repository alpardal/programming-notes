module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Generic (class Generic, gEq)

data Vector = Vector Number Number

derive instance genericVector :: Generic Vector

instance eqVector :: Eq Vector where
  eq = gEq

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ eq (Vector 3.0 4.0) (Vector 3.0 4.01)

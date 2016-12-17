-- import all functions:
import Data.List

-- choose functions to import:
import Control.Monad (liftM)

-- import all but:
import Data.Map hiding (fold)

-- qualified import:
import qualified Data.Map

-- qualified with alias:
import qualified Data.Char as Char

-- import type, but not constructors:
import Data.Maybe(Maybe())

-- choose constructors to import:
import Data.Maybe(Maybe(Just))

-- import type + all constructors:
import Data.Maybe(Maybe(..))

{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE ParallelListComp #-}

import GHC.Exts

-- with multiple generators:

[ (x,y) | x <- [0 .. 6], y <- [x .. 6]]

-- with guards:

doubleOdds list = [ 2 * x | x <- list, odd x ]

-- with pattern matching:

[ clientName x | x@(GovOrg _ _) <- listOfClients ]

-- with local bindings:

[ sqrt v | (x,y) <- [(1,2),(3,8)], let v = x*x + y*y]

-- EXTENSIONS:

  -- TransformListComp

    [x*y | x <- [-1,1,-2], y <- [1,2,3], then reverse]
    -- => [-6,-4,-2,3,2,1,-3,-2,-1]

    -- using GHC.Exts (`then f by e`):

    [x*y | x <- [-1,1,-2], y <- [1,2,3], then sortWith by x]
    -- => [-2,-4,-6,-1,-2,-3,1,2,3]

    -- using GHC.Exts (`then group by e using f`):
    [ (the p, m) | x <- [-1,1,-2]
                 , y <- [1,2,3]
                 , let m = x*y ,
                 , let p = m > 0
                 , then group by p using groupWith ]
    -- => [(False,[-1,-2,-3,-2,-4,-6]),(True,[1,2,3])]


  -- ParallelListComp:

    [ x*y | x <- [1,2,3], y <- [1,2,3] ] -- traditional nesting
    -- => [1,2,3,2,4,6,3,6,9]
    [ x*y | x <- [1,2,3] | y <- [1,2,3] ] -- zipping
    -- => [1,4,9]

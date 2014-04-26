{- |
Module      :  Util.L1
Copyright   :  (c) Alberto Ruiz 2011
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

L1 Tools.

-}
-----------------------------------------------------------------------------

module Util.L1 (
    l1SolveO,
    l1SolveU
) where

import Numeric.LinearAlgebra
import Util.Misc(Mat,Vec,debug,vec)
import Numeric.LinearProgramming

-- | L1 solution of an overconstrained linear system Ax=b using linear programming.
-- We find the x producing minimum |Ax-b|_1.
l1SolveO :: Mat -> Vec -> Vec
l1SolveO a y = debug "L1" (const j) $ vec (take n x)
  where
    n = cols a
    m = rows a
    eye = ident m
    c = fromBlocks [[ a,-eye],
                    [-a,-eye]]
    d = vjoin [y,-y]
    p = Dense $ zipWith (:<=:) (toLists c) (toList d)
    Optimal (j,x) = simplex (Minimize (replicate n 0 ++ replicate m 1)) p (map Free [1..(n+m)])


-- | L1 solution of an underconstrained linear system Ax=b using linear programming.
-- We find the minimum |x|_1 such that Ax=b.
l1SolveU :: Mat -> Vec -> Vec
l1SolveU a y = debug "L1" (const j) $ vec (take n x)
  where
    n = cols a
    c1 = map (\k ->  [ 1#k, -1#k+n] :<=: 0) [1..n]
    c2 = map (\k ->  [-1#k, -1#k+n] :<=: 0) [1..n]
    c3 = zipWith (:==:) (map sp $ toRows a) (toList y)
    sp v = zipWith (#) (toList v) [1..]
    p = Sparse (c1 ++ c2 ++ c3)
    Optimal (j,x) = simplex (Minimize (replicate n 0 ++ replicate n 1)) p (map Free [1..(2*n)])


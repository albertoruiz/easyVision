module Util.NLHash(bestOrientation) where

import Util.Geometry ( Segment(Segment), Point(Point) )
import Numeric.LinearAlgebra.HMatrix
import Util.Misc ( degree )


--------------------------------------------------------------------------------
{-

import Devel.Contours.NLHash
import Contours
import Util.Rotation
import Util.Misc(degree)
import Numeric.LinearAlgebra

f o = o - bestOrientation (asSegments $ transPol (rot3 (-o*degree)) $ fst $ pentominos!!3)

main = do
    print $ maximum . map abs $ map f $ toList (linspace 1000 (-17,15))


8.580034510868728e-2

error < 0.1 degree

-}

--------------------------------------------------------------------------------

bestOrientation :: [Segment] -> Double
bestOrientation segs = d
  where
    ds = deltas segs
    rs = findRot ds    
    d = refineExtreme rs (minIndex rs) - 44
    deltas = map delta
      where
        delta (Segment (Point x1 y1) (Point x2 y2)) = fromList [x2-x1, y2-y1]

--------------------------------------------------------------------------------

findRot :: [Vector Double] -> Vector Double
findRot [] = vjoin [konst 1 44, konst 0.95 1, konst 1 44]
findRot ds = tr m #> ones
  where
    mds = fromRows ds
    m = abs (mds <> rots1) `minEvery` abs (mds <> rots2)
    ones = konst 1 (rows m)
    minEvery a b = cond a b a a b
    rots1 = tr $ fromLists [ [cos (a*degree) , sin (a*degree)] | a <- [-44 .. 45] ]
    rots2 = tr $ fromLists [ [-sin (a*degree), cos (a*degree)] | a <- [-44 .. 45] ]

--------------------------------------------------------------------------------

refineExtreme :: Vector Double -> Int -> Double
refineExtreme v k = fromIntegral k + subBin (v `cycAt` (k-1)) (v `cycAt` k) (v `cycAt` (k+1))
  where
    subBin p q r = (p-r) / (2*(p+r-2*q))
    cycAt u j = u ! (j `mod` size u)

--------------------------------------------------------------------------------


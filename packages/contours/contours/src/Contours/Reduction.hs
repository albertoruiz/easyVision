-----------------------------------------------------------------------------
{- |
Module      :  Contours.Reduction
Copyright   :  (c) Alberto Ruiz 2007-12
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Contour Reduction

-}
-----------------------------------------------------------------------------

module Contours.Reduction (
    reduceDP,
    smoothPolyline,
    reducePolyline
)
where

import Contours.Base
import Data.List(maximumBy)
import Data.Function(on)
import Util.Misc(splitEvery)
import Util.Statistics(mean)


reduceDP :: Double -> Polyline -> Polyline
-- ^ Reduce a polyline using the Douglas-Peucker algorithm.

reduceDP eps (Open ps) = Open (f ps)
  where
    f list = a: douglasPeucker' eps a b list
        where a = head list
              b = last list

reduceDP eps (Closed ps) = Closed (f ps)
  where
    f (a:b:ls) = b : case criticalPoint eps b a ls of
        Nothing -> [b]
        Just c  -> left ++ right where
            (l,_:r) = break (==c) ls
            left = douglasPeucker' eps b c l
            right = douglasPeucker' eps c a r
    f _ = undefined

--------------------------------

douglasPeucker' :: Double -> Point -> Point -> [Point] -> [Point]
douglasPeucker' eps a b ls = case criticalPoint eps a b ls of
    Nothing -> [b]
    Just c  -> left ++ right where
        (l,_:r) = break (==c) ls
        left = douglasPeucker' eps a c l
        right = douglasPeucker' eps c b r

--------------------------------

perpDist2 :: Point -> Point -> (Point -> Double, Double)
perpDist2 (Point x1 y1) (Point x2 y2) = (f,l2)
  where
    lx = x2-x1
    ly = y2-y1
    l2 = lx*lx+ly*ly
    f (Point x3 y3) = d2
      where
        d2 = p2 - a'*a'/l2
        p2   = px*px + py*py
        px   = x3-x1
        py   = y3-y1
        a'   = lx*px+ly*py

--------------------------------

criticalPoint :: Double -> Point -> Point -> [Point] -> Maybe Point
criticalPoint _   _  _  [] = Nothing
criticalPoint eps p1 p2 p3s = r where
    (f,_) = perpDist2 p1 p2
    p3 = maximumBy (compare `on` f) p3s
    r = if f p3 > eps*eps
        then Just p3
        else Nothing

--------------------------------------------------------------------------------

smoothPolyline :: Int -> Polyline -> Polyline
smoothPolyline k (Closed p) = Closed (smooth k p)
smoothPolyline k (Open   p) = Closed (smooth k p)

smooth :: Int -> [Point] -> [Point]
smooth k = map meanPoint . splitEvery k
  where
    meanPoint pts = Point (mean $ map px pts) (mean $ map py pts)
    px (Point x _) = x
    py (Point _ y) = y

--------------------------------------------------------------------------------

reducePolyline :: Polyline -> Polyline
-- ^ a reasonable default: @reduceDP 1/500 . smoothPolyline 3@
reducePolyline = reduceDP (1/500) . smoothPolyline 3


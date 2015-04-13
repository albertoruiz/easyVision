{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
{- |
Module      :  Contours.Base
Copyright   :  (c) Alberto Ruiz 2007-11
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Basic operations with polylines.

-}
-----------------------------------------------------------------------------

module Contours.Base (
    Point(..), Polyline(..), Segment(..),
    convexHull,
    perimeter,
    area, orientedArea, rev, 
    asSegments, transPol,
    pentominos,
    segmentIntersection,
    intersectionLineSegment,
    bisector,
    tangentsTo,
    isLeft,
    minkowskiConvex, minkowskiComponents
)
where


import Data.List(sortBy)
import Numeric.LinearAlgebra.HMatrix
import Util.Homogeneous hiding (flipx)
import Data.Function(on)
import Util.Geometry
import Util.Polygon
import Util.Misc(Mat)


-- | (for an open polyline is the length)
perimeter :: Polyline -> Double
perimeter (Open l) = perimeter' l
perimeter (Closed l) = perimeter' (last l:l)

perimeter' :: [Point] -> Double
perimeter' (a:b:rest) = distPoints a b + perimeter' (b:rest)
perimeter' _ = 0

area :: Polyline -> Double
area = abs . orientedArea

-- | Oriented area of a closed polyline. The clockwise sense is positive in the x-y world frame (\"floor\",z=0) and negative in the camera frame.
orientedArea :: Polyline -> Double
orientedArea (Open _) = error "undefined orientation of open polyline"
orientedArea (Closed l) = orientation (Polygon l)

rev :: Polyline -> Polyline
rev (Closed ps) = Closed (reverse ps)
rev (Open ps) = Open (reverse ps)

----------------------------------------------------------------------

{-
perpDistAux :: Int -> Int -> Double -> Int -> Int -> Int -> Int -> Double
perpDistAux lx ly l2 x1 y1 x3 y3 = d2 where
    d2 = p2 - a'*a'/l2
    p2   = fromIntegral $ px*px + py*py
    px   = x3-x1
    py   = y3-y1
    a'   = fromIntegral $ lx*px+ly*py

perpDist (Pixel x1 y1) (Pixel x2 y2) = (f,l2) where
    lx = x2-x1
    ly = y2-y1
    l2 = fromIntegral $ lx*lx+ly*ly
    f (Pixel x3 y3) = perpDistAux lx ly l2 x1 y1 x3 y3


criticalPoint eps p1 p2 [] = Nothing

criticalPoint eps2 p1 p2 p3s = r where
    (f,l2) = perpDist p1 p2
    p3 = maximumBy (compare `on` f) p3s
    r = if f p3 > eps2
        then Just p3
        else Nothing
-}
----------------------------------------------------------------------

transPol :: Util.Misc.Mat -> Polyline -> Polyline
transPol t (Closed ps) = Closed $ map l2p $ ht t (map p2l ps)
transPol t (Open ps)   = Open   $ map l2p $ ht t (map p2l ps)

p2l :: Point -> [Double]
p2l (Point x y) = [x,y]

l2p :: [Double] -> Point
l2p [x,y] = Point x y
l2p _ = error "l2p"

----------------------------------------------------------

flipx :: Polyline -> Polyline
flipx = transPol (diagl[-1,1,1])

pentominos :: [(Polyline,String)]
pentominos =
    [ (Closed $ reverse [Point 0 0, Point 0 1, Point 5 1, Point 5 0], "I")
    , (flipx $ Closed $ [Point 0 0, Point 0 1, Point 3 1, Point 3 2, Point 4 2, Point 4 0], "L")
    , (Closed $ reverse [Point 0 0, Point 0 1, Point 3 1, Point 3 2, Point 4 2, Point 4 0], "J")
    , (Closed $ reverse [Point 1 0, Point 1 1, Point 0 1, Point 0 2, Point 1 2, Point 1 3,
                         Point 2 3, Point 2 2, Point 3 2, Point 3 1, Point 2 1, Point 2 0], "X")
    , (Closed $ reverse [Point 0 0, Point 0 3, Point 1 3, Point 1 1, Point 3 1, Point 3 0], "V")
    , (Closed $ reverse [Point 0 0, Point 0 1, Point 1 1, Point 1 3, Point 2 3, Point 2 1, Point 3 1, Point 3 0], "T")
    , (flipx $ Closed $ [Point 0 0, Point 0 3, Point 2 3, Point 2 1, Point 1 1, Point 1 0], "P")
    , (Closed $ reverse [Point 0 0, Point 0 3, Point 2 3, Point 2 1, Point 1 1, Point 1 0], "B")
    , (flipx $ Closed $ [Point 0 2, Point 0 3, Point 2 3, Point 2 1, Point 3 1, Point 3 0, Point 1 0, Point 1 2], "Z")
    , (Closed $ reverse [Point 0 2, Point 0 3, Point 2 3, Point 2 1, Point 3 1, Point 3 0, Point 1 0, Point 1 2], "S")
    , (Closed $ reverse [Point 0 0, Point 0 2, Point 1 2, Point 1 1, Point 2 1, Point 2 2, Point 3 2, Point 3 0], "U")
    , (flipx $ Closed $ [Point 0 0, Point 0 1, Point 2 1, Point 2 2, Point 3 2, Point 3 1, Point 4 1, Point 4 0], "Y")
    , (Closed $ reverse [Point 0 0, Point 0 1, Point 2 1, Point 2 2, Point 3 2, Point 3 1, Point 4 1, Point 4 0], "Y'")
    , (flipx $ Closed $ [Point 0 1, Point 0 3, Point 1 3, Point 1 2, Point 3 2, Point 3 1,
                         Point 2 1, Point 2 0, Point 1 0, Point 1 1], "F")
    , (Closed $ reverse [Point 0 1, Point 0 3, Point 1 3, Point 1 2, Point 3 2, Point 3 1,
                         Point 2 1, Point 2 0, Point 1 0, Point 1 1], "Q")
    , (flipx $ Closed $ [Point 0 1, Point 0 2, Point 2 2, Point 2 1, Point 4 1, Point 4 0, Point 1 0, Point 1 1], "N")
    , (Closed $ reverse [Point 0 1, Point 0 2, Point 2 2, Point 2 1, Point 4 1, Point 4 0, Point 1 0, Point 1 1], "N'")
    , (Closed $ reverse [Point 0 1, Point 0 3, Point 1 3, Point 1 2, Point 2 2, Point 2 1,
                         Point 3 1, Point 3 0, Point 1 0, Point 1 1], "W")    
    ]

----------------------------------------------------------------------

impossible :: a
impossible = undefined

convexHull :: [Point] -> [Point]
convexHull ps | length ps > 3 = go [q0] rs
              | otherwise     = ps
  where
    q0:qs = sortBy (compare `on` (\(Point x y) -> (y,x))) ps
    rs = sortBy (compare `on` (ncosangle q0)) qs

    go [p] [x,q]                     = [p,x,q]
    go [p] (x:q:r)   | isLeft p x q  = go [x,p] (q:r)
                     | otherwise     = go [p]   (q:r)
    go (p:c) [x]     | isLeft p x q0 = x:p:c
                     | otherwise     =   p:c
    go (p:c) (x:q:r) | isLeft p x q  = go (x:p:c)   (q:r)
                     | otherwise     = go c       (p:q:r)
    
    go _ _ = impossible
    
    ncosangle p1@(Point x1 _) p2@(Point x2 _) = (x1-x2) / distPoints p1 p2


tangentsTo :: Point -> Polyline -> Maybe (Point,Point)
tangentsTo q x = r
  where
    r | length can == 2 = Just (a,b)
      | otherwise = Nothing
    [a,b] = can
    can = canTans q x

canTans :: Point -> Polyline -> [Point]
canTans q x = can
  where
    xs = cl2 $ convexHull $ polyPts $ x
    can = [a | t@(_,a,_) <- zipWith3 (,,) xs (tail xs) (tail (tail xs)), f t ]
    f (a,b,c) = isLeft q a b && not (isLeft q b c)
              || not (isLeft q a b) && isLeft q b c
    cl2 (a:b:zs) = a:b:zs++[a,b]
    cl2 _ = impossible

--------------------------------------------------------------------------------

minkowskiComponents :: Polygon -> Polygon -> [Polygon]
minkowskiComponents d p = [ minkowskiConvex di pj | di <- convexComponents d, pj <- convexComponents p]
  
minkowskiConvex :: Polygon -> Polygon -> Polygon
minkowskiConvex a b = Polygon $ convexHull
    [ Point (-x1+x2) (-y1+y2) | Point x1 y1 <- polygonNodes a
                              , Point x2 y2 <- polygonNodes b ]


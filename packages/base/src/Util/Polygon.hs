module Util.Polygon(
    insidePolygon,
    simplePolygon,
    convexComponents
) where

import Util.Geometry
import Data.Maybe(catMaybes)
import qualified Data.Vector as V
import Data.Function(on)
import Data.List(sortBy)
import Util.Misc(pairsWith, mkCyclic, cycAt)

insidePolygon :: Point -> Polygon -> Bool
insidePolygon pt c = odd (length (filter (cut pt) (polygonSides c)))
  where
    cut p@(Point x y) (Segment q1@(Point x1 y1) q2@(Point x2 y2))
        | (y1-y)*(y-y2) < 0 = False
        | x < min x1 x2 = False
        | x > max x1 x2 = True
        | y2 < y1 = isLeft p q1 q2
        | otherwise = isLeft p q2 q1

--------------------------------------------------------------------------------

simplePolygon :: Polygon -> Bool
simplePolygon pol = null $ catMaybes [segmentIntersection a b | (a@(Segment p q),b@(Segment r s)) <- pairsWith (,) ss, q/=r, s/=p]
  where
    ss = polygonSides pol

--------------------------------------------------------------------------------

cumsums :: Polygon -> [Double]
cumsums (Polygon ps) = ss
  where
    ds = map segmentLength $ asSegments (Closed (ps++ps))
    ss = scanl (+) 0 ds

-- works with negative area
cuts :: Polygon -> [((Int, Int), Double)]
cuts p@(Polygon ps) = g $ pairsWith f [0.. l-1]
  where
    l = length ps
    f i j = ((i,j), dout/din)
      where
        din  = (cs V.! j - cs V.! i) `min` (cs V.! (i+l) - cs V.! j)
        dout = distPoints (xs V.! i) (xs V.!j)
    cs = V.fromList $ cumsums p
    xs = V.fromList ps
    g = take 1 . sortBy (compare `on` snd) . filter h
    h ((i,j), _) = (concave i || concave j) && simplePolygon p1 && simplePolygon p2 && nocut 
      where
        nocut = abs (ap - area p1 - area p2)/ ap < 1E-10
        ap = area p
        (p1,p2) = splitCont p i j
    cyc = cycAt c
      where
        c = mkCyclic ps
    concave k = not $ isLeft (cyc (k-1)) (cyc k) (cyc (k+1))
    area = abs . orientation
    
convexComponents :: Polygon -> [Polygon]
convexComponents (Polygon ps)
    = if length ps < 4 || null t
        then [Polygon ps]
        else convexComponents p1 ++ convexComponents p2
  where
    [((i,j),_)] = t
    t = cuts (Polygon ps)
    (p1,p2) = splitCont (Polygon ps) i j


splitCont :: Polygon -> Int -> Int -> (Polygon, Polygon)
splitCont (Polygon ps) i j = (p1,p2)
  where
    a = take (i+1) ps
    c = drop j ps
    b = drop i $ take (j+1) ps
    p1 = Polygon (a++c)
    p2 = Polygon b



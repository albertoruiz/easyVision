module Contours.Resample(
    resample)
where

import Contours.Base
import Contours.Normalization(momentsContour)
import Util.Geometry
import Data.Maybe(catMaybes)
import Data.Function(on)
import Data.List(sortBy)
import Util.Misc(rotateLeft)

resample :: Int -> Polyline -> [Point]
resample n c = take n $ splitDelta delta delta xs
  where
    s = zip [0..] (asSegments c)
    ((k, Segment _ _), r)
       | null ints = (s!!0, Point x y) -- FIXME
       | otherwise = head ints
    ints = sortBy (compare `on` px) $ catMaybes (map (si (Segment (Point (x-100) y) (Point (100+x) y))) s)
    p1:p2:ps = rotateLeft k (polyPts c)
    pts = r:p2:ps++[p1,r]
    ss = asSegments (Closed pts)
    ds = map segmentLength ss
    xs = zip ss ds
    delta = perimeter c / fromIntegral n
    (x,y,_,_,_) = momentsContour (polyPts c)
    si a (q,b) = fmap ((,) (q,b)) (segmentIntersection' a b)
    px (_,Point xx _) = xx

splitDelta :: Double -> Double -> [(Segment, Double)] -> [Point]
splitDelta dl delta ((Segment p q,l):xs)
    | l > dl = p : splitDelta delta delta ((Segment r q, l-dl) : xs)
    | otherwise = p: tail (splitDelta (dl-l) delta xs)
  where
    r = interPoint (dl/l) p q
splitDelta _ _ _ = undefined


{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
{- |
Module      :  Features.Segments
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Extraction of polylines from segments

-}
-----------------------------------------------------------------------------

module Features.Segments (segmentsToPolylines) where

import Data.Array hiding ((//))
import Data.List(sortBy,(\\))
import Data.Graph
import Data.Tree
import Debug.Trace
import Vision
import Numeric.LinearAlgebra
import Features.Polyline
import ImagProc

vector = fromList :: [Double] -> Vector Double

debug x = trace (show x) x

------------------------------------------------------------------------------


distances segs = do
    let n = length segs
    i <- [1 .. n-1]
    j <- [i+1 .. n]
    let Segment {extreme1 = a1, extreme2 = a2} = segs!!(i-1)
    let Segment {extreme1 = b1, extreme2 = b2} = segs!!(j-1)
    ds <- [(distPoints a1 b1,(i,j)),
           (distPoints a1 b2,(i,-j)),
           (distPoints a2 b1,(-i,j)),
           (distPoints a2 b2,(-i,-j))]
    return ds


createGraph l dsegs = buildG (-l,l) (edgelist ++ map (\(a,b)->(b,a)) edgelist)
    where edgelist = map snd dsegs ++ [(i,-i)|i<- [1..l]]

tk segs a | a > 0     = extreme1 (segs!!(a-1))
          | otherwise = extreme2 (segs!!(-a-1))

extend g (f:ps) = case (g!f) \\ ps of
    []   -> (f:ps)
    a:as -> extend g (a:f:ps)

-- | Given a distance threshold, it tries to link a list of segments to create a list of polylines.
segmentsToPolylines :: Double -> [Segment] -> [Polyline]
segmentsToPolylines r segs = (map (improvePoints.putType.map (tk segs).{-debug.-}okInit.cleanOdd) (filter ((1<).length) pol))
    where dists = filter ((<r).fst) (distances segs)
          g = createGraph (length segs) dists
          pol = map (extend g.reverse.extend g.return.rootLabel) (components g)
          putType l | isClosed r l = Closed l
                    | otherwise    = Open   l

okInit l@(a:b:rest) | a == -b          = l
                    | a == - last rest = b:rest ++ [a]
                    | otherwise        = b:rest

isClosed r p = distPoints (head p) (last p) < r

-- | Replaces the extremes of consecutive segments by the intersection of such segments
improvePoints :: Polyline -> Polyline
improvePoints (Open l)              = Open   (betterPoints $ auxLines' l)
improvePoints (Closed l@(a:b:rest)) = clockwise $ Closed (betterPoints $ auxLines' (l++[a,b]))

auxLines' [] = []
auxLines' (Point x1 y1: Point x2 y2 :rest) = cross (vector [x1,y1,1]) (vector [x2,y2,1]) : auxLines' rest

betterPoints [_] = []
betterPoints (a:b:rest) = toPoint (inHomog (cross a b)) : betterPoints (b:rest)
    where toPoint v = let [x,y] = toList v in Point x y

cleanOdd [a,b] | a == -b = [a,b]
               | otherwise = [a]
cleanOdd (a:b:c:rest) | b==(-a) || b==(-c) = a: cleanOdd (b:c:rest)
                      | otherwise = cleanOdd (a:c:rest)

clockwise p@(Closed l) | orientation p < 0 = p  -- in the camera frame clockwise orientation is negative
                       | otherwise         = Closed (reverse l)


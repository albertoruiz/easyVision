-----------------------------------------------------------------------------
{- |
Module      :  Contours.Clipping
Copyright   :  (c) PARP Research Group, University of Murcia, 2012
License     :  GPL
Maintainer  :  Alberto Ruiz
Stability   :  provisional

Haskell interface to efficient clipping of arbitrary polygons.

Implementation in C by Adrián Amor Martínez.

Algorithm by G. Greiner, K. Hormann, ACM Transactions on Graphics.

Additional info and support for degenerate cases by Pedro E. López de Teruel.

-}
-----------------------------------------------------------------------------

module Contours.Clipping (
    -- * Normal use
    clip, ClipMode(..),
    -- * Extra information
    preclip,
    deltaContour
)
where

import Util.Geometry
import Contours(area,orientedArea,rev,momentsContour,tangentsTo)
import Data.List(minimumBy,sort)
import Data.Function(on)
--import Util.Debug(debug)
import Contours.ClipRaw

--------------------------------------------------------------------------------

clip :: ClipMode -> Polyline -> Polyline -> [Polyline]
-- ^ set operations for polygons
clip m a b = map (fst.fst) (fixOrientation p)
  where
    (p,_,_) = preclip m a b

--------------------------------------------------------------------------------

deltaContour
    :: Polyline -> Polyline
    -> [((Polyline,Double),[Polyline])]
deltaContour a b
    | ins == 0  = fixOrientation p
    | ins == 1  = [((undefined,-da), op)]  -- FIXME understand sign change
    | ins == 2  = [((undefined,da), op)]
    | otherwise = deltaDisj a b
  where
    (p,_n,ins) = {-debug "INS" (\(_,_,i)->i) $ -} preclip ClipXOR a b
    clo xs = xs ++ [head xs]
    op = [Open $ clo $ polyPts b]
    da = abs (area a - area b)



fixOrientation :: ([(Polyline, [Int])],Int) -> [((Polyline,Double),[Polyline])]
fixOrientation (xs,np) = zp' ++ zn'
  where
    ys = map (step2 . step1) xs
    (zp,zn) = splitAt np ys
    zp' = map (fixOri (-1)) zp
    zn' = map (fixOri   1 ) zn
    fixOri s ((p,ar),os) | signum ar == signum s = ((p,ar),os)
                         | otherwise = ((rev p,-ar), map rev os)


step1 :: (Polyline, [Int]) -> ((Polyline,Double), [(Point, Int)])
step1 (ps, os) = ((ps,oa), pos)
  where
    pos = zip (polyPts ps) os
    oa = orientedArea ps

-- extract fragments
step2 :: ((Polyline,Double), [(Point, Int)]) -> ((Polyline,Double), [Polyline])
step2 (poa, pos) = (poa, map Open (fragments pos))
  where
    fragments :: [(Point,Int)] -> [[Point]]
    fragments = map (map fst) . frags . filter ((/=1).snd)
      where
        frags [] = []
        frags (p:xs) | null ts = error $ "FRAG " ++ show pos
                     | otherwise = (p : rs ++ [q]) : frags ys
          where
            (rs,ts) = span ((==2).snd) xs
            q:ys = ts



centroid :: Polyline -> Point
centroid p = Point mx my
  where
    (mx,my,_,_,_) = momentsContour (polyPts p)

splitAtP :: Polyline -> (Point, Point) -> (Polyline, Polyline)
splitAtP (Closed ps) (a,b) = (r1,r2)
  where
    psp = zip [0..] ps
    pos p = fst . minimumBy (compare `on` distPoints p . snd) $ psp
    [j1,j2] = sort (map pos [a,b])
    n = length ps -1
    is1 = [j1..j2]
    is2 = [j2..n]++[0..j1]
    r1 = Open (map (ps!!) is1)
    r2 = Open (map (ps!!) is2)
splitAtP _ _ = impossible
    
deltaDisj
    :: Polyline -> Polyline
    -> [((Polyline, Double), [Polyline])]
deltaDisj a b = r
  where
    ca = centroid a
    Just t@(t1,t2) = tangentsTo ca b
    (r1,r2) = splitAtP b t
    [f1,f2] = map g [r1,r2]
    [a1,a2] = map area [f1,f2]
    g (Open ps) = Closed (ca:ps)
    g _ = impossible
    -- FIXME empirical acceptable value
    am = - distPoints (centroid b) ca * distPoints t1 t2 / 4
    r = if a1 < a2
          then [((f1,-am),[r1]), ((f2,am),[r2])]
          else [((f2,-am),[r2]), ((f1,am),[r1])]

impossible :: t
impossible = undefined


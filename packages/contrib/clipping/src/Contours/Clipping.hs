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
    clip, ClipMode(..),
    preclip,
    deltaContour
)
where

import ImagProc.Base
import Contours.Base(orientedArea,rev)
import Util.Misc(debug)

import Contours.ClipRaw

--------------------------------------------------------------------------------

clip :: ClipMode -> Polyline -> Polyline -> [Polyline]
-- ^ set operations for polygons
clip m a b = map (fst.fst) (fixOrientation p)
  where
    (p,_,_) = preclip m a b

--------------------------------------------------------------------------------

deltaContour :: Polyline -> Polyline -> [((Polyline,Double),[Polyline])]
deltaContour a b | ins == 0 = checkDob b $ fixOrientation p
                 | otherwise = debug "disj!" (const ()) donothing
  where
    (p,n,ins) = preclip ClipXOR a b
    donothing = [((undefined,0),[Open $ clo $ polyPts b])]
    clo xs = xs ++ [head xs]

checkDob :: Polyline -> [((Polyline,Double),[Polyline])] -> [((Polyline,Double),[Polyline])]
checkDob x d | dob = debug "Fragmented!" (const ()) donothing
             | otherwise = d
  where
    dob = any ((>1).length.snd) d
    donothing = [((undefined,0),[Open $ clo $ polyPts x])]
    clo xs = xs ++ [head xs]

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


{-# LANGUAGE ForeignFunctionInterface #-}
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

-}
-----------------------------------------------------------------------------

module Contours.Clipping (
    clip, ClipMode(..),
    preclip,
    deltaContour
)
where

import ImagProc.Base
import ImagProc.Ipp.Core
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import System.IO.Unsafe(unsafePerformIO)
import Control.Applicative((<$>))
import Data.Packed.Vector(takesV,fromList,toList)
import Contours.Base(orientedArea,rev)
import Util.Misc(impossible,debug)

--------------------------------------------------------------------------------

foreign import ccall "clip" c_clip
    :: Ptr Double -> Ptr Double -> CInt
    -> Ptr Double -> Ptr Double -> CInt
    -> Ptr (Ptr Double) -> Ptr (Ptr Double) -> Ptr (Ptr CInt) -> Ptr (Ptr (CInt))
    -> Ptr (CInt) -> Ptr (CInt) -> Ptr (CInt) -> CInt
    -> IO CInt

--------------------------------------------------------------------------------

data ClipMode = ClipIntersection
              | ClipUnion
              | ClipDifference
              | ClipXOR
              deriving (Enum, Show)

clip :: ClipMode -> Polyline -> Polyline -> [Polyline]
-- ^ set operations for polygons
clip m a b = map (fst.fst) (fixOrientation p)
  where
    (p,_,_) = preclip m a b

--------------------------------------------------------------------------------

deltaContour :: Polyline -> Polyline -> [((Polyline,Double),[Polyline])]
deltaContour a b | n > 0     = fixOrientation p
                 | otherwise = disj ins
  where
    (p,n,ins) = preclip ClipXOR a b
    ([(a',_),(b',_)],1) = p
    disj 0 = debug "disj!" (const ()) [((undefined,0),[])]
    disj _ = [((err ,s),[b''])]
      where
        s = orientedArea b' - orientedArea a'
        b'' = Open . clo . polyPts $ b'
        err = impossible "disj-circ"
        clo xs = xs++[head xs]


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
        frags (p:xs) = (p : rs ++ [q]) : frags ys
          where
            (rs,q:ys) = span ((==2).snd) xs


--------------------------------------------------------------------------------

preclip :: ClipMode -> Polyline -> Polyline -> (([(Polyline, [Int])],Int),Int,Int)
-- ^ interface to C function to compute set operation and origin for each vertex
preclip mode (Closed a') (Closed b') = unsafePerformIO $ do
    ppxs <- malloc
    ppys <- malloc
    ppos <- malloc
    ppl  <- malloc
    pn   <- malloc
    pnp  <- malloc
    pins <- malloc

    let a = a' ++ [head a']
        b = b' ++ [head b']   
        nc = length a
        ns = length b

    cx <- newArray (map px a)
    cy <- newArray (map py a)
    sx <- newArray (map px b)
    sy <- newArray (map py b)
    
    --peekArray nc cx >>= print
    --peekArray nc cy >>= print
    --peekArray ns sx >>= print
    --peekArray ns sy >>= print

    _ok <- c_clip cx cy (fi nc) sx sy (fi ns) ppxs ppys ppos ppl pn pnp pins (2^fromEnum mode)

    --print _ok

    n <- ti <$> peek pn
    --print n
    np <- ti <$> peek pnp
    --print np
    
    insideCode <- ti <$> peek pins
    --print insideCode
    
    pl <- peek ppl
    ls <- map ti <$> peekArray n pl
    --print ls

    let tot = sum ls
 
    pxs <- peek ppxs
    pys <- peek ppys
    pos <- peek ppos

    xs <- peekArray tot pxs
    ys <- peekArray tot pys
    os <- map ti `fmap` peekArray tot pos

    -- print (ls,os)
    
    -- mapM_ print $ zip3 xs ys os

    -- provisional
    let vxs = map (init . toList) $ takesV ls (fromList xs)
        vys = map (init . toList) $ takesV ls (fromList ys)
        vos = map (init . toList) $ takesV ls (fromList os)
        r | n > 0 = (zip (zipWith f vxs vys) vos, np)
          | otherwise = ([],0)
          where f as bs = Closed $ zipWith Point as bs
    
    free pxs
    free pys
    free pos
    free pl

    free ppxs
    free ppys
    free ppos
    free ppl
    free pn
    
    return (fixEmpty r mode insideCode a' b', n, insideCode)

preclip _ _ _ = error "clip on open polylines not defined"

--------------------------------------------------------------------------------

fixEmpty :: ([(Polyline, [Int])], Int)
         -> ClipMode -> Int -> [Point] -> [Point] -> ([(Polyline, [Int])], Int)
fixEmpty ([],_) ClipIntersection 0 _ _ = ([],0)
fixEmpty ([],_) ClipIntersection 1 a _ = ([cla a],1)
fixEmpty ([],_) ClipIntersection 2 _ b = ([clb b],1)
fixEmpty ([],_) ClipUnion        0 a b = ([cla a,clb b],2)
fixEmpty ([],_) ClipUnion        1 _ b = ([clb b],1)
fixEmpty ([],_) ClipUnion        2 a _ = ([cla a],1)
fixEmpty ([],_) ClipDifference   0 a _ = ([cla a],1)
fixEmpty ([],_) ClipDifference   1 _ _ = ([],0)
fixEmpty ([],_) ClipDifference   2 a b = ([cla a,clb b],1)
fixEmpty ([],_) ClipXOR          _ a b = ([cla a,clb b],1)

fixEmpty r ClipXOR _ _ _ = r
fixEmpty (cs,_) _ _ _ _ = (cs, length cs)

cl :: Int -> [Point] -> (Polyline, [Int])
cl x a = (Closed a, replicate x (length a))

cla,clb :: [Point] -> (Polyline, [Int])
cla = cl 1
clb = cl 2


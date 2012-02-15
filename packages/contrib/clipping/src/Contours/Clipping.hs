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
    clip
)
where

import ImagProc.Base
import ImagProc.Ipp.Core
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import System.IO.Unsafe(unsafePerformIO)

foreign import ccall "clip" c_clip
    :: Ptr Double -> Ptr Double -> CInt
    -> Ptr Double -> Ptr Double -> CInt
    -> Ptr (Ptr Double) -> Ptr (Ptr Double) -> Ptr (Ptr (CInt)) 
    -> Ptr (CInt) -> CInt -> IO CInt


clip :: Polyline -> Polyline -> [Polyline]
-- ^ compute the intersection of two polygons
clip a b = unsafePerformIO $ do
    ppsx <- malloc
    ppsy <- malloc
    ppl <- malloc
    pn  <- malloc
    let p2lx (Point x y) = [x]
        p2ly (Point x y) = [y]
        rep l = l ++ [head l]
        lax = concatMap p2lx (rep $ polyPts a)
        lay = concatMap p2ly (rep $ polyPts a)
        lbx = concatMap p2lx (rep $ polyPts b)
        lby = concatMap p2ly (rep $ polyPts b)
        na = length lax
        nb = length lbx
    aax <- newArray lax
    aay <- newArray lay
    abx <- newArray lbx
    aby <- newArray lby
    peekArray (na) aax >>= print
    peekArray (na) aay >>= print
    peekArray (nb) abx >>= print
    peekArray (nb) aby >>= print
    ok <- c_clip aax aay (fi na) abx aby (fi nb) ppsx ppsy ppl pn (fi 1)
    n <- peek pn
    print n
    --polysx <- splitPlaces (peekArray (5) ppl) (peekArray (5) ppsx)
    --polysy <- splitPlaces (peekArray (5) ppl) (peekArray (5) ppsy)
    free ppsx
    free ppsy
    free ppl
    free pn
    return [] --zip (polysx polysy)


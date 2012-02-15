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
    :: Ptr Double -> CInt -> Ptr Double -> CInt
    -> Ptr (Ptr Double) -> Ptr (Ptr (CInt)) -> Ptr (CInt)
    -> IO CInt


clip :: Polyline -> Polyline -> [Polyline]
-- ^ compute the intersection of two polygons
clip a b = unsafePerformIO $ do
    pps <- malloc
    ppl <- malloc
    pn  <- malloc
    let p2l (Point x y) = [x,y]
        rep l = l -- ++ [head l]
        la = concatMap p2l (rep $ polyPts a)
        lb = concatMap p2l (rep $ polyPts b)
        na = length la `div` 2
        nb = length lb `div` 2
    aa <- newArray la
    ab <- newArray lb
    peekArray (2*na) aa >>= print
    peekArray (2*nb) ab >>= print
    ok <- c_clip aa (fi na)  ab (fi nb) pps ppl pn
    n <- peek pn
    print n
    free pps
    free ppl
    free pn
    return []


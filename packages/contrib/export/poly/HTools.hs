-- example of exporting a contour processing function to C

{-# LANGUAGE ForeignFunctionInterface #-}

module HTools(fun, hfun) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr(Ptr,castPtr)
import Foreign.Storable
import Foreign.Marshal.Array

import Contours

--------------------------------------------------------------------------------

type HFun = Int -> Polyline -> Polyline

fun :: HFun
fun n = Closed . resample n

--------------------------------------------------------------------------------

type CFun
    =  CInt
    -> CInt -> Ptr Point
    -> Ptr CInt -> Ptr (Ptr Point)
    -> IO Int

cfun :: CFun
cfun s n ps pm pps = do
    x <- peekArray (fromIntegral n) ps

    let y   = fun (fromIntegral s) (Closed x)
        rps = polyPts y
        m   = length rps

    poke pm (fromIntegral m)
    x <- newArray rps
    poke pps x
    return 0

hfun = cfun
foreign export ccall hfun :: CFun


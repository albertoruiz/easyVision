{-# LANGUAGE ForeignFunctionInterface #-}

module HTools(
    fun, hfun
) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr(Ptr,castPtr)
import Foreign.Storable
import Foreign.Marshal.Array

import Contours
import Contours.CLK
import Numeric.LinearAlgebra.HMatrix

--------------------------------------------------------------------------------

fun :: HFun
fun proto target = (lkdAlignment result, lkdHomography result)
  where
    result = lkd (noDeformable target) proto

--------------------------------------------------------------------------------

type HFun = Polyline -> Polyline -> (Polyline,Matrix Double)

type CFun =  CInt
          -> Ptr Point
          -> CInt
          -> Ptr Point
          -> Ptr CInt
          -> Ptr (Ptr Point)
          -> Ptr Double
          -> IO Int

foreign export ccall hfun :: CFun
hfun = cfun

--------------------------------------------------------------------------------

cfun n1 ps1 n2 ps2 pm pps pmat = do
    p <- peekArray (fromIntegral n1) ps1
    t <- peekArray (fromIntegral n2) ps2 

    let (y,mat)   = fun (Closed p) (Closed t)
        rps = polyPts y
        m   = length rps

    poke pm (fromIntegral m)
    x <- newArray rps
    poke pps x
    
    pokeArray pmat (toList (flatten mat))
    return 0


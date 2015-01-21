-- example of exporting a contour processing function to C

{-# LANGUAGE ForeignFunctionInterface #-}

module HTools(fun, hfun,
              funInit, hfunInit) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr(Ptr,castPtr)
import Foreign.Storable
import Foreign.Marshal.Array

import Contours

--------------------------------------------------------------------------------

funInit :: HInit
funInit = return ()

fun :: HFun
fun n = Closed . resample n

--------------------------------------------------------------------------------

(hfunInit, hfun) = mkC (funInit, fun)
foreign export ccall hfun :: CFun
foreign export ccall hfunInit :: CInit

--------------------------------------------------------------------------------

type HInit = IO ()
type CInit = IO ()

type HFun = Int -> Polyline -> Polyline

type CFun =  CInt
          -> CInt -> Ptr Point
          -> Ptr CInt -> Ptr (Ptr Point)
          -> IO Int

--------------------------------------------------------------------------------

mkC :: (HInit, HFun) -> (CInit, CFun)
mkC (init, f) = (cInit, cFun)
  where
    cInit = init

    cFun s n ps pm pps = do
      x <- peekArray (fromIntegral n) ps
      
      let y = f (fromIntegral s) (Closed x)
          rps = polyPts y
          m = length rps

      poke pm (fromIntegral m)
      x <- newArray rps
      poke pps x
      return 0


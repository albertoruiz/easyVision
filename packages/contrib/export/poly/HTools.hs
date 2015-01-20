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

-- function with arguments, some of them typically provided by initialization

funInit :: HInit
funInit = return ()

fun :: HFun
fun = Closed . resample 8

--------------------------------------------------------------------------------

(hfunInit, hfun) = mkC (funInit, fun)
foreign export ccall hfun :: CFun
foreign export ccall hfunInit :: CInit

--------------------------------------------------------------------------------

type HInit = IO ()
type CInit = IO ()

type HFun = Polyline -> Polyline

type CFun =  CInt
          -> Ptr Point
          -> Ptr CInt
          -> Ptr (Ptr Point)
          -> IO Int

--------------------------------------------------------------------------------

mkC :: (HInit, HFun) -> (CInit, CFun)
mkC (init, f) = (cInit, cFun)
  where
    cInit = init

    cFun n ps pm pps = do
      x <- peekArray (fromIntegral n) ps
      
      let y = f (Closed x)
          rps = polyPts y
          m = length rps

      poke pm (fromIntegral m)
      x <- newArray rps
      poke pps x
      return 0


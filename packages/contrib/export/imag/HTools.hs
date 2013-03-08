{-# LANGUAGE ForeignFunctionInterface #-}

module HTools(fun, hfun, funpar, hfunpar, funparInit, hfunparInit) where

import Foreign.C.Types
import Foreign.Ptr(Ptr,castPtr)
import Foreign.Marshal.Array(copyArray)
import ImagProc
import ImagProc.Ipp.Core
import GHC.Stable

----------------------

import Numeric.LinearAlgebra

--------------------------------------------------------------------------------

-- pure function

fun = notI

hfun = mkC fun

foreign export ccall hfun :: GrayFun

----------------------------------------------

-- function with arguments, provided by initialization

funpar :: (Matrix Double) -> (ImageGray -> ImageGray)
funpar m img = warp 128 (size img) m img

funparInit :: IO (Matrix Double)
funparInit = loadMatrix "data.txt"

(hfunparInit, hfunpar) = mkCP funparInit funpar

foreign export ccall hfunpar :: GrayFunP
foreign export ccall hfunparInit :: MkInit

--------------------------------------------------------------------------------

{- Simple Haskell -> C image processing interface

   We assume that
   - the function does not change the image size
   - the number of columns is multiple of 32
   - it works with full ROI
-}

type GrayFun =  Ptr CUChar -> CInt -> CInt  -- source and size (rows columns)
             -> Ptr CUChar                  -- destination
             -> IO Int                      -- exit code

mkC :: (ImageGray -> ImageGray) -> GrayFun
mkC fun pS sr sc pD = do
    let r = ti sr; c = ti sc
    img @ (G Img {ptr = p}) <- image (Size r c)
    copyArray (castPtr p) pS (r*c)
    
    let G Img {ptr = d} = fun img

    copyArray pD (castPtr d) (r*c)
    return 0

--------------------------------------------------------------------------------

-- With an additional parameter as an opaque reference to Haskell value

type Init a = IO a -> IO (StablePtr a)

type MkInit = IO (Ptr ())

type GrayFunP =  Ptr()                       -- auxiliary argument
              -> Ptr CUChar -> CInt -> CInt  -- source and size (rows columns)
              -> Ptr CUChar                  -- destination
              -> IO Int                      -- exit code

mkCP :: IO a -> (a -> ImageGray -> ImageGray) -> (MkInit, GrayFunP)
mkCP init fun = (cInit, cFun)
  where
    cInit = do
      a <- init
      p <- newStablePtr a
      return (castStablePtrToPtr p)

    cFun x pS sr sc pD = do
      let r = ti sr; c = ti sc
      img @ (G Img {ptr = p}) <- image (Size r c)
      copyArray (castPtr p) pS (r*c)
    
      a <- deRefStablePtr (castPtrToStablePtr x)
    
      let G Img {ptr = d} = fun a img

      copyArray pD (castPtr d) (r*c)
      return 0



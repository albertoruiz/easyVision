{-# LANGUAGE ForeignFunctionInterface #-}

module HTools(fun, hfun) where

import Foreign.C.Types
import Foreign.Ptr(Ptr,castPtr)
import Foreign.Marshal.Array(copyArray)
import ImagProc
import ImagProc.Ipp.Core

fun = notI

hfun = mkC fun

foreign export ccall hfun :: GrayFun

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


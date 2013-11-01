{-# LANGUAGE ForeignFunctionInterface #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Ipp.Wrappers
Copyright   :  (c) Alberto Ruiz 2006-10
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Special interface to some IPP functions not yet automatically generated.

-}
-----------------------------------------------------------------------------


module ImagProc.Ipp.Wrappers where

import Foreign
import Foreign.C.Types

foreign import ccall "auxIpp.h ippGetStatusString" ippGetStatusString :: Int -> IO (Ptr CChar)

foreign import ccall "auxIpp.h ippSetNumThreads" ippSetNumThreads :: Int -> IO Int

foreign import ccall "auxIpp.h auxWarpPerspective_32f_C1R"
     warpPerspective32f :: Ptr Word8 -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr Word8 -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr Double -> Int ->
                           IO Int

foreign import ccall "auxIpp.h auxWarpPerspective_8u_C1R"
     warpPerspectiveGray :: Ptr Word8 -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr Word8 -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr Double -> Int ->
                           IO Int

foreign import ccall "auxIpp.h auxWarpPerspective_8u_C3R"
     warpPerspectiveRGB :: Ptr Word8 -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr Word8 -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr Double -> Int ->
                           IO Int

foreign import ccall "auxIpp.h auxResize_32f_C1R"
     c_resize32f :: Ptr Word8 -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr Word8 -> Int ->
                           Int -> Int -> Int -> Int ->
                           Int ->
                           IO Int

foreign import ccall "auxIpp.h auxResize_8u_C1R"
     c_resize8u :: Ptr Word8 -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr Word8 -> Int ->
                           Int -> Int -> Int -> Int ->
                           Int ->
                           IO Int

foreign import ccall "auxIpp.h auxResize_8u_C3R"
     c_resize8u3 :: Ptr Word8 -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr Word8 -> Int ->
                           Int -> Int -> Int -> Int ->
                           Int ->
                           IO Int

foreign import ccall "auxIpp.h auxDCTFwd_32f_C1R"
     auxDCTFwd_32f_C1R :: Ptr Float -> Int ->
                          Int -> Int -> Int -> Int ->
                          Ptr Float -> Int ->
                          Int -> Int -> Int -> Int ->
                          IO Int

foreign import ccall "auxIpp.h auxDCTInv_32f_C1R"
     auxDCTInv_32f_C1R :: Ptr Float -> Int ->
                          Int -> Int -> Int -> Int ->
                          Ptr Float -> Int ->
                          Int -> Int -> Int -> Int ->
                          IO Int

foreign import ccall "auxInpainting_8u_C1R"
    auxInpainting_8u_C1R :: Float -> Int ->
                            Ptr Word8 -> Int ->
                            Ptr Word8 -> Int ->
                            Ptr Float -> Int ->
                            Ptr Word8 -> Int ->
                            Int -> Int -> Int -> Int ->
                            IO Int


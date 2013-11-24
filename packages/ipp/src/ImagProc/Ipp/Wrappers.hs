{-# LANGUAGE ForeignFunctionInterface #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Ipp.Wrappers
Copyright   :  (c) Alberto Ruiz 2006-10
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Special Interface to some IPP functions not yet automatically generated.

-}
-----------------------------------------------------------------------------


module ImagProc.Ipp.Wrappers where

import Foreign
import Foreign.C.Types
import Image.Core

foreign import ccall "auxIpp.h ippGetStatusString" ippGetStatusString :: CInt -> IO (Ptr CChar)

foreign import ccall "auxIpp.h ippSetNumThreads" ippSetNumThreads :: CInt -> IO CInt

foreign import ccall "auxIpp.h auxWarpPerspective_32f_C1R"
     warpPerspective32f :: Ptr Word8 -> CInt -> CInt -> CInt ->
                           CInt -> CInt -> CInt -> CInt ->
                           Ptr Word8 -> CInt ->
                           CInt -> CInt -> CInt -> CInt ->
                           Ptr Double -> CInt ->
                           IO CInt

foreign import ccall "auxIpp.h auxWarpPerspective_8u_C1R"
     warpPerspectiveGray :: Ptr Word8 -> CInt -> CInt -> CInt ->
                           CInt -> CInt -> CInt -> CInt ->
                           Ptr Word8 -> CInt ->
                           CInt -> CInt -> CInt -> CInt ->
                           Ptr Double -> CInt ->
                           IO CInt

foreign import ccall "auxIpp.h auxWarpPerspective_8u_C3R"
     warpPerspectiveRGB :: Ptr Word8 -> CInt -> CInt -> CInt ->
                           CInt -> CInt -> CInt -> CInt ->
                           Ptr Word8 -> CInt ->
                           CInt -> CInt -> CInt -> CInt ->
                           Ptr Double -> CInt ->
                           IO CInt

foreign import ccall "auxIpp.h auxResize_32f_C1R"
     c_resize32f :: CInt -> RawImage Float (RawImage Float (IO CInt))

foreign import ccall "auxIpp.h auxResize_8u_C1R"
     c_resize8u :: CInt -> RawImage Word8 (RawImage Word8 (IO CInt))

foreign import ccall "auxIpp.h auxResize_8u_C3R"
     c_resize8u3 :: CInt -> RawImage Word24 (RawImage Word24 (IO CInt))

foreign import ccall "auxIpp.h auxDCTFwd_32f_C1R"
     auxDCTFwd_32f_C1R :: Ptr Float -> CInt ->
                          CInt -> CInt -> CInt -> CInt ->
                          Ptr Float -> CInt ->
                          CInt -> CInt -> CInt -> CInt ->
                          IO CInt

foreign import ccall "auxIpp.h auxDCTInv_32f_C1R"
     auxDCTInv_32f_C1R :: Ptr Float -> CInt ->
                          CInt -> CInt -> CInt -> CInt ->
                          Ptr Float -> CInt ->
                          CInt -> CInt -> CInt -> CInt ->
                          IO CInt

foreign import ccall "auxInpainting_8u_C1R"
    auxInpainting_8u_C1R :: Float -> CInt ->
                            Ptr Word8 -> CInt ->
                            Ptr Word8 -> CInt ->
                            Ptr Float -> CInt ->
                            Ptr Word8 -> CInt ->
                            CInt -> CInt -> CInt -> CInt ->
                            IO CInt


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
import Image.Devel

foreign import ccall "auxIpp.h ippGetStatusString" ippGetStatusString :: CInt -> IO (Ptr CChar)

foreign import ccall "auxIpp.h ippSetNumThreads" ippSetNumThreads :: CInt -> IO CInt

foreign import ccall "auxIpp.h auxWarpPerspective_32f_C1R"
     warpPerspective32f :: Ptr Double -> CInt -> CInt
                        -> RawImage Float (RawImage Float (IO CInt))

foreign import ccall "auxIpp.h auxWarpPerspective_8u_C1R"
     warpPerspective8u :: Ptr Double -> CInt -> CInt
                       -> RawImage Word8 (RawImage Word8 (IO CInt))

foreign import ccall "auxIpp.h auxWarpPerspective_8u_C3R"
     warpPerspective8u3 :: Ptr Double -> CInt -> CInt
                        -> RawImage Word24 (RawImage Word24 (IO CInt))

foreign import ccall "auxResize_32f_C1R"
     c_resize32f :: RawImage Float (RawImage Float (IO CInt))

foreign import ccall "auxResize_8u_C1R"
     c_resize8u :: RawImage Word8 (RawImage Word8 (IO CInt))

foreign import ccall "auxResize_8u_C3R"
     c_resize8u3 :: RawImage Word24 (RawImage Word24 (IO CInt))

foreign import ccall "auxResize_32f_C1R_NN"
     c_resize32f_NN :: RawImage Float (RawImage Float (IO CInt))

foreign import ccall "auxResize_8u_C1R_NN"
     c_resize8u_NN :: RawImage Word8 (RawImage Word8 (IO CInt))

foreign import ccall "auxResize_8u_C3R_NN"
     c_resize8u3_NN :: RawImage Word24 (RawImage Word24 (IO CInt))


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


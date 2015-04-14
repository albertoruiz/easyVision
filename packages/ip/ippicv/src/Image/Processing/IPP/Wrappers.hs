{-# LANGUAGE ForeignFunctionInterface #-}

-----------------------------------------------------------------------------
{- |
Module      :  Image.Processing.IPP.Wrappers
Copyright   :  (c) Alberto Ruiz 2006-10
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Special Interface to some IPP functions not yet automatically generated.

-}
-----------------------------------------------------------------------------


module Image.Processing.IPP.Wrappers where

import Foreign
import Foreign.C.Types
import Image.Devel
--import Image.Processing.IPP.Structs

{-

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
-}

foreign import ccall "auxResize_32f_C1R"
     c_resize32f :: RawImage Float (RawImage Float (IO CInt))

foreign import ccall "auxResize_8u_C1R"
     c_resize8u :: RawImage Word8 (RawImage Word8 (IO CInt))

foreign import ccall "auxResize_8u_C3R"
     c_resize8u3 :: RawImage Word24 (RawImage Word24 (IO CInt))

{-

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

foreign import ccall "ippiCompare_32f_C1Rx"
    ippiCompare_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> CInt -> IO CInt
ippiCompare_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSz ippCmpOp = do
    proiSize <- new roiSz
    r <- ippiCompare_32f_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize ippCmpOp
    free proiSize
    return r

foreign import ccall "ippiCompareC_32f_C1Rx"
    ippiCompareC_32f_C1Rx :: Ptr Float -> Int -> Float -> Ptr Word8 -> Int -> Ptr IppiSize -> CInt -> IO CInt
ippiCompareC_32f_C1R pSrc srcStep value pDst dstStep roiSz ippCmpOp = do
    proiSize <- new roiSz
    r <- ippiCompareC_32f_C1Rx pSrc srcStep value pDst dstStep proiSize ippCmpOp
    free proiSize
    return r

-}


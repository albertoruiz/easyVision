-- generated automatically by adapter.hs

{-# LANGUAGE ForeignFunctionInterface #-}

module Image.Processing.IPP.Adapt where

import Foreign
import Foreign.C.Types
import Image.Processing.IPP.Structs

foreign import ccall "ippiCopy_8u_C1Rx"
    ippiCopy_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiCopy_8u_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiCopy_8u_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiRGBToGray_8u_C3C1Rx"
    ippiRGBToGray_8u_C3C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiRGBToGray_8u_C3C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiRGBToGray_8u_C3C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r


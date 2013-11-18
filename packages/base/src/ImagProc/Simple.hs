{-# LANGUAGE ForeignFunctionInterface #-}


module ImagProc.Simple (
    yuyv2rgb, yuyv2gray,
    yuv2rgb, yuv2gray,
    exampleInvert
) where

import Image.Core
import Image.Devel

yuyv2rgb :: ImageYCbCr -> ImageRGB
yuyv2rgb = undefined

yuyv2gray :: ImageYCbCr -> ImageGray
yuyv2gray = undefined

yuv2rgb :: ImageYUV -> ImageRGB
yuv2rgb = undefined

yuv2gray :: ImageYUV -> ImageGray
yuv2gray = undefined

foreign import ccall "exampleInvert"
    c_exampleInvert :: Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> CInt
                    -> Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> CInt
                    -> IO CInt

exampleInvert :: ImageGray -> ImageGray
exampleInvert (G x) = unsafePerformIO $ do
    G res <- image $ size (G x)
    appG c_exampleInvert (G x) `appG` G res // checkFFI "exampleInvert" [x,res]
    return (G res) 


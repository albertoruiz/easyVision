{-# LANGUAGE ForeignFunctionInterface #-}


module ImagProc.Simple (
    yuyv2rgb, yuyv2gray,
    yuv2rgb, yuv2gray,
    yuv2yuyv,
    exampleInvert
) where

import Image.Core

--------------------------------------------------------------------------------

yuv2yuyv :: ImageYUV -> ImageYCbCr
yuv2yuyv = wrap11 c_yuv2yuyv

foreign import ccall "yuv2yuyv" c_yuv2yuyv :: Wrap11

--------------------------------------------------------------------------------

yuyv2rgb :: ImageYCbCr -> ImageRGB
yuyv2rgb = wrap11 c_yuyv2rgb

foreign import ccall "yuyv2rgb" c_yuyv2rgb :: Wrap11

--------------------------------------------------------------------------------

yuyv2gray :: ImageYCbCr -> ImageGray
yuyv2gray = undefined

--------------------------------------------------------------------------------

yuv2rgb :: ImageYUV -> ImageRGB
yuv2rgb = yuyv2rgb . yuv2yuyv

--------------------------------------------------------------------------------

yuv2gray :: ImageYUV -> ImageGray
yuv2gray = undefined

--------------------------------------------------------------------------------

exampleInvert :: ImageGray -> ImageGray
exampleInvert = wrap11 c_exampleInvert

foreign import ccall "exampleInvert" c_exampleInvert :: Wrap11


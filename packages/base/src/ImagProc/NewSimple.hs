{-# LANGUAGE ForeignFunctionInterface #-}


module ImagProc.NewSimple (
    yuyv2rgb, yuyv2gray,
    yuv2rgb, yuv2gray,
    yuv2yuyv,
    exampleInvert
) where

import Image.Internal

--------------------------------------------------------------------------------

yuv2yuyv :: Image Word8 -> Image Word16
yuv2yuyv = wrap11 "kk" c_yuv2yuyv

foreign import ccall "yuv2yuyv" c_yuv2yuyv :: Wrap11 Word8 Word16

--------------------------------------------------------------------------------

yuyv2rgb :: Image Word16 -> Image RGB
yuyv2rgb = wrap11 "kk" c_yuyv2rgb

foreign import ccall "yuyv2rgb" c_yuyv2rgb :: Wrap11 Word16 RGB

--------------------------------------------------------------------------------

--yuyv2gray :: ImageYCbCr -> ImageGray
yuyv2gray = undefined

--------------------------------------------------------------------------------

--yuv2rgb :: ImageYUV -> ImageRGB
yuv2rgb = yuyv2rgb . yuv2yuyv

--------------------------------------------------------------------------------

--yuv2gray :: ImageYUV -> ImageGray
yuv2gray = undefined

--------------------------------------------------------------------------------

exampleInvert :: Image Word8 -> Image Word8
exampleInvert = wrap11 "kk" c_exampleInvert

foreign import ccall "exampleInvert" c_exampleInvert :: Wrap11 Word8 Word8


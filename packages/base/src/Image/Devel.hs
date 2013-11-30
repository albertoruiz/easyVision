{-# LANGUAGE ForeignFunctionInterface #-}


module Image.Devel(
    RawImage, appI,
    Wrap11, wrap11,
    fi, ti,
    (//), checkFFI,
    flattenImage,
    getDataFileName,
    module Image.Core,
    convert,
    yuyv2rgb, yuv2rgb, yuv2yuyv, gray2rgb,
    mpSize, parseSize,
    unsafePerformIO,
    CInt(..), Storable, Ptr
) where

import Paths_hVision_base
import Foreign.C.Types(CInt(..))
import Image.Core
import System.IO.Unsafe(unsafePerformIO)
import Foreign.Storable(Storable(..))
import Control.Monad(when)
import Foreign.Ptr(Ptr)
import Util.Misc((//))
import Image.Convert

appI :: RawImage p t -> Image p -> t
appI f img = f (ptrAt img (Pixel 0 0)) (fi.step $ img) (g r1) (g r2) (g c1) (g c2)
  where
    g x = (fi . x . roi) img
    r1 (ROI r _ _ _) = r
    r2 (ROI _ r _ _) = r
    c1 (ROI _ _ c _) = c
    c2 (ROI _ _ _ c) = c

type RawImage p t = Ptr p -> CInt -> CInt -> CInt -> CInt -> CInt -> t

type Wrap11 p q = RawImage p (RawImage q (IO CInt))

wrap11 :: Storable b => String -> Wrap11 a b -> Image a -> Image b
wrap11 msg f x = unsafePerformIO $ do
    r <- newImage undefined (size x)
    withImage x $ withImage r $ checkFFI msg $
        appI f x `appI` r
    return r

checkFFI :: String -> IO CInt -> IO ()
checkFFI msg f = do
    err <- f
    when (err/=0)  (error $ "error in foreign function " ++ msg)

fi :: Int -> CInt
fi = fromIntegral
ti :: CInt -> Int
ti = fromIntegral

--------------------------------------------------------------------------------

parseSize :: String -> Size
parseSize s | 'x' `elem` s = f s
            | otherwise    = mpSize (read s)
  where
    f = h . words . map g
    g 'x' = ' '
    g y = y
    h [a,b] = Size (read b) (read a)
    h _ = error "parseSize error"

-- | Computes a 4\/3 \'good\' size for both mplayer and IPP. mpSize 20 = 640x480
mpSize :: Int -> Size
mpSize k | k > 0     = Size (k*24) (k*32)
         | otherwise = error "mpSize"

--------------------------------------------------------------------------------

yuv2yuyv :: ImageYUV -> ImageYCbCr
yuv2yuyv = wrap11 "yuv2yuyv" c_yuv2yuyv

foreign import ccall "yuv2yuyv" c_yuv2yuyv :: Wrap11 YUV YCbCr

yuyv2rgb :: ImageYCbCr -> ImageRGB
yuyv2rgb = wrap11 "yuyv2rgb" c_yuyv2rgb

foreign import ccall "yuyv2rgb" c_yuyv2rgb :: Wrap11 YCbCr RGB

yuv2rgb :: ImageYUV -> ImageRGB
yuv2rgb = yuyv2rgb . yuv2yuyv

gray2rgb :: Image I8u -> Image I8u3
gray2rgb = wrap11 "gray2rgb" c_gray2rgb

foreign import ccall "gray2rgb" c_gray2rgb :: Wrap11 I8u I8u3

--------------------------------------------------------------------------------

flattenImage :: Image I8u3 -> Image I8u
flattenImage im = im { szpix = 1, size = Size h (3*w), roi = ROI r1 r2 (c1*3) (c2*3) }
  where
    Size h w = size im
    ROI r1 r2 c1 c2 = roi im


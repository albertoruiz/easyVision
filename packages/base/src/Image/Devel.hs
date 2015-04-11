{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}



module Image.Devel(
    RawImage, appI,
    Wrap11, wrap11,
    RawImageS, appS,
    Wrap11S, wrap11S,
    MAT, withCMatrix, appMat,
    VEC, withVector, appVec,
    fi, ti,
    (//), checkFFI,
    flattenImage,
    getInclude,
    module Image.Core,
    convert,
    yuyv2gray, yuyv2rgb, yuv2rgb, yuv2yuyv, gray2rgb, rgb2gray,
    gray2float, float2gray,
    BPix(..),
    constantImage,
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
import Data.List.Split(splitOn)
import Image.Convert
import Foreign.ForeignPtr(withForeignPtr)
import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
import Numeric.LinearAlgebra.HMatrix(Matrix,flatten,rows,cols,Vector)
import qualified Numeric.LinearAlgebra.HMatrix as M
import Numeric.LinearAlgebra.Devel(fi,orderOf, MatrixOrder(..),unsafeToForeignPtr)

type RawImage p t = Ptr p -> CInt -> CInt -> CInt -> CInt -> CInt -> t

appI :: RawImage p t -> Image p -> t
appI f img = f (ptrAt img (Pixel 0 0)) (fi.step $ img) (g r1) (g r2) (g c1) (g c2)
  where
    g x = (fi . x . roi) img
    r1 (ROI r _ _ _) = r
    r2 (ROI _ r _ _) = r
    c1 (ROI _ _ c _) = c
    c2 (ROI _ _ _ c) = c

type RawImageS p t = Ptr p -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> t

appS :: RawImageS p t -> Image p -> t
appS f img = f (ptrAt img (Pixel 0 0)) (fi h) (fi w) (fi.step $ img) (g r1) (g r2) (g c1) (g c2)
  where
    g x = (fi . x . roi) img
    r1 (ROI r _ _ _) = r
    r2 (ROI _ r _ _) = r
    c1 (ROI _ _ c _) = c
    c2 (ROI _ _ _ c) = c
    Size h w = size img

type Wrap11 p q = RawImage p (RawImage q (IO CInt))

wrap11 :: Storable b => String -> Wrap11 a b -> Image a -> Image b
wrap11 msg f x = unsafePerformIO $ do
    r <- newImage undefined (size x)
    withImage x $ withImage r $ checkFFI msg $
        f `appI` x `appI` r
    return r

type Wrap11S p q = RawImageS p (RawImageS q (IO CInt))

wrap11S :: Storable b => String -> Wrap11S a b -> Image a -> Image b
wrap11S msg f x = unsafePerformIO $ do
    r <- newImage undefined (size x)
    withImage x $ withImage r $ checkFFI msg $
        f `appS` x `appS` r
    return r

--------------------------------------------------------------------------------

type MAT t = CInt -> CInt -> Ptr Double -> t

withCMatrix :: Matrix Double -> IO b -> IO b
withCMatrix m act
   | orderOf m == RowMajor = withForeignPtr fp $ \_ -> act
   | otherwise = error "withMatrix ColumnMajor"
  where
    (fp,_,_) = unsafeToForeignPtr (flatten m)

appMat :: MAT t -> Matrix Double -> t
appMat f m = f r c p
  where
    r = fi (rows m)
    c = fi (cols m)
    (fp,_,_) = unsafeToForeignPtr (flatten m)
    p = unsafeForeignPtrToPtr fp

--------------------------------------------------------------------------------

type VEC t = CInt -> Ptr Double -> t

withVector :: Vector Double -> IO b -> IO b
withVector v act = withForeignPtr fp $ \_ -> act
  where
    (fp,_,_) = unsafeToForeignPtr v

appVec :: VEC t -> Vector Double -> t
appVec f v = f n p
  where
    n = fi (M.size v)
    (fp,_,_) = unsafeToForeignPtr v
    p = unsafeForeignPtrToPtr fp

--------------------------------------------------------------------------------

checkFFI :: String -> IO CInt -> IO ()
checkFFI msg f = do
    err <- f
    when (err/=0)  (error $ "error in foreign function " ++ msg)

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

yuyv2gray :: ImageYCbCr -> Image I8u
yuyv2gray = wrap11 "yuyv2gray" c_yuyv2gray
foreign import ccall "yuyv2gray" c_yuyv2gray :: Wrap11 YCbCr I8u

yuv2rgb :: ImageYUV -> ImageRGB
yuv2rgb = yuyv2rgb . yuv2yuyv

gray2rgb :: Image I8u -> Image I8u3
gray2rgb = wrap11 "gray2rgb" c_gray2rgb
foreign import ccall "gray2rgb" c_gray2rgb :: Wrap11 I8u I8u3

rgb2gray :: Image I8u3 -> Image I8u
rgb2gray = wrap11 "rgb2gray" c_rgb2gray
foreign import ccall "rgb2gray" c_rgb2gray :: Wrap11 I8u3 I8u

gray2float :: Image I8u -> Image Float
gray2float = wrap11 "gray2float" c_gray2float
foreign import ccall "gray2float" c_gray2float :: Wrap11 I8u I32f

float2gray :: Image Float -> Image I8u
float2gray = wrap11 "float2gray" c_float2gray
foreign import ccall "float2gray" c_float2gray :: Wrap11 I32f I8u

foreign import ccall "setroi8u" setroi8u :: I8u -> RawImage I8u (IO CInt)
foreign import ccall "setroi8u3" setroi8u3 :: I8u -> I8u -> I8u -> RawImage I8u3 (IO CInt)
foreign import ccall "setroi32f" setroi32f :: Float -> RawImage Float (IO CInt)

iosetroig :: String -> (p -> RawImage p (IO CInt)) -> p -> Image p -> IO ()
iosetroig msg f v x = withImage x $ checkFFI msg $ f v `appI` x

--------------------------------------------------------------------------------

getInclude :: IO String
getInclude = do
    fw <- getDataFileName "src/Image/Devel/wrappers.h"
    return $ "include-dirs: " ++ head (splitOn "/wrappers.h" fw)

--------------------------------------------------------------------------------

flattenImage :: Image I8u3 -> Image I8u
flattenImage im = im { szpix = 1, size = Size h (3*w), roi = ROI r1 r2 (c1*3) (c2*3) }
  where
    Size h w = size im
    ROI r1 r2 c1 c2 = roi im

--------------------------------------------------------------------------------

class Storable p => BPix p
  where
    ioset :: p -> Image p -> IO()

instance BPix I8u
  where
    ioset = iosetroig "iosetroi8u" setroi8u

instance BPix Float
  where
    ioset = iosetroig "iosetroi32f" setroi32f

instance BPix I8u3
  where
    ioset = iosetroig "iosetroi8u3" setroi8u3'
      where
        setroi8u3' (Word24 r g b) = setroi8u3 r g b

constantImage :: BPix p => p -> Size -> Image p
constantImage v sz = unsafePerformIO $ do
    r <- newImage undefined sz
    ioset v r
    return r


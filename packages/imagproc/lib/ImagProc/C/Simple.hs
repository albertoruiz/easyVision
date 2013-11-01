{-# LANGUAGE ForeignFunctionInterface #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.C.Simple
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Interface to a few simple algorithms implemented in C.

-}
-----------------------------------------------------------------------------


module ImagProc.C.Simple where

import ImagProc.Ipp.Core
import ImagProc.Generic(clone,set)
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import System.IO.Unsafe(unsafePerformIO)
import Foreign(Word8)

-- | Explores an image and returns a list of pixels (as [row,column]) where the image is greater than 0.0.
getPoints32f :: Int -> ImageFloat -> [Pixel]
getPoints32f mx (F im) = unsafePerformIO $ do
    r <- mallocArray (2*mx)
    ptot <- malloc
    ok <- c_getPoints32f (castPtr (ptr im)) (step im) 
                   (r1 (vroi im)) (r2 (vroi im)) (c1 (vroi im)) (c2 (vroi im))
                   mx ptot r
    touchForeignPtr (fptr im)
    tot <- peek ptot
    hp <- peekArray (fromIntegral tot) r
    free ptot
    free r
    return (partitPixel hp)

partitPixel :: [CInt] -> [Pixel]
partitPixel [] = []
partitPixel [a] = error "partitPixel on a list with odd number of entries"
partitPixel (r:c:l) = Pixel (fromIntegral r) (fromIntegral c) : partitPixel l


foreign import ccall "Simple/simple.h getPoints32f"
    c_getPoints32f :: Ptr Float -> Int -> Int -> Int -> Int -> Int ->
                      Int -> Ptr CInt -> Ptr CInt -> IO Int

-----------------------------------------------------------------------------

-- | Histogram of the 256 possible configurations of 3x3 image patches thresholded by the central pixel. Works inside the image ROI.
lbp :: Int       -- ^ threshold tolerance
    -> ImageGray -- ^ source image
    -> [Int]     -- result
lbp th (G im) = unsafePerformIO $ do
    hist <- mallocArray 256
    lbp8u th (ptr im) (step im) (r1 (vroi im)) (r2 (vroi im)) (c1 (vroi im)) (c2 (vroi im)) hist
        // checkIPP "lbp" [im]
    r <- peekArray 256 hist
    free hist
    return $ map fromIntegral r

-- normalized lbp histogram
lbpN t im = map ((*sc).fromIntegral) (tail h) where
    h = lbp t im
    ROI r1 r2 c1 c2 = theROI im
    sc = (256.0::Double) / fromIntegral ((r2-r1-1)*(c2-c1-1))

foreign import ccall "Simple/simple.h lbp8u"
     lbp8u :: Int -> Ptr Word8 -> Int -> Int -> Int -> Int -> Int -> Ptr CInt -> IO Int

-----------------------------------------------------------------------------

-- | to do
hsvCodeTest :: Int -> Int -> Int -> ImageRGB -> ImageRGB
hsvCodeTest b g w (C orig) = unsafePerformIO $ do
    C im <- clone (C orig)
    hsvcodeTest b g w (ptr im) (step im) (r1 (vroi im)) (r2 (vroi im)) (c1 (vroi im)) (c2 (vroi im))
        // checkIPP "hsvcodeTest" [im]
    return (C im)

-- | to do
hsvCode :: Int -> Int -> Int -> ImageRGB -> ImageGray
hsvCode b g w (C im) = unsafePerformIO $ do
    G r <- image (isize im)
    set 0 (theROI (G r)) (G r)
    hsvcode b g w
            (ptr im) (step im)
            (ptr r) (step r)
            (r1 (vroi im)) (r2 (vroi im)) (c1 (vroi im)) (c2 (vroi im))
            // checkIPP "hsvcode" [im]
    return $ modifyROI (const (vroi im)) (G r)

foreign import ccall "Simple/simple.h hsvcodeTest"
     hsvcodeTest :: Int -> Int -> Int -> Ptr Word8 -> Int -> Int -> Int -> Int -> Int -> IO Int
foreign import ccall "Simple/simple.h hsvcode"
     hsvcode :: Int -> Int -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Int -> Int -> Int -> Int -> IO Int

-------------------------------------------------------------------------------

genLocMax f mx th (F im1) (F im2) (F im3) = unsafePerformIO $ do
    r <- mallocArray (2*mx)
    ptot <- malloc
    ok <- f (castPtr (ptr im1)) (step im1)
            (castPtr (ptr im2)) (step im2)
            (castPtr (ptr im3)) (step im3)
            (r1 (vroi im3)) (r2 (vroi im3)) (c1 (vroi im3)) (c2 (vroi im3))
            mx ptot th r
    touchForeignPtr (fptr im1)
    touchForeignPtr (fptr im2)
    touchForeignPtr (fptr im3)
    tot <- peek ptot
    hp <- peekArray (fromIntegral tot) r
    free ptot
    free r
    return (partitPixel hp)


-- | Explores 3 images in a scale space pyramid and returns the local maximum
--   (in the roi of im3, points selected on im2)
localMaxScale3 :: Int -> Float -> ImageFloat -> ImageFloat -> ImageFloat -> [Pixel]
localMaxScale3 = genLocMax c_localMaxScale3

foreign import ccall "Simple/simple.h localMaxScale3"
    c_localMaxScale3 :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr Float -> Int
                        -> Int -> Int -> Int -> Int ->
                        Int -> Ptr CInt -> Float -> Ptr CInt -> IO Int

-------------

-- | A special version of localMaxScale3 for images with filtermaximum
localMaxScale3Simplified :: Int -> Float -> ImageFloat -> ImageFloat -> ImageFloat -> [Pixel]
localMaxScale3Simplified = genLocMax c_localMaxScale3Simplified

foreign import ccall "Simple/simple.h localMaxScale3Simplified"
    c_localMaxScale3Simplified
                        :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr Float -> Int
                        -> Int -> Int -> Int -> Int ->
                        Int -> Ptr CInt -> Float -> Ptr CInt -> IO Int

-------------------------------------------------------------------------------

-- | simple C function for benchmarks
foreign import ccall "Simple/simple.h csum32f"
    c_sum32f :: Ptr Float -> Int
                -> Int -> Int -> Int -> Int
                -> IO Double

csum32f (F im) = unsafePerformIO $ do
    let ROI r1 r2 c1 c2 = vroi im
    r <- c_sum32f (castPtr (ptr im)) (step im) r1 r2 c1 c2
    return r

-----------------------------------------------------------------------------

-- | Histogram of local gradient directions, useful for interest point descriptors
histoDir :: ImageFloat -- ^ gm (modulus of gradient)
         -> ImageFloat -- ^ gx
         -> ImageFloat -- ^ gy
         -> Double     -- ^ sigma for smoothing
         -> Pixel      -- ^ smoothing center
         -> Int        -- ^ number of bins (e.g. 36, 8, etc.)
         -> [Double]
histoDir (F ga) (F gx) (F gy) sigma (Pixel r c) n = unsafePerformIO $ do
    hist <- mallocArray n
    c_histodir (ptr ga) (step ga) (ptr gx) (step gx) (ptr gy) (step gy)
               (r1 (vroi ga)) (r2 (vroi ga)) (c1 (vroi ga)) (c2 (vroi ga))
               sigma r c n hist
               // checkIPP "c_histodir" [ga,gx,gy]
    res <- peekArray n hist
    free hist
    return res


foreign import ccall "Simple/simple.h histodir"
     c_histodir :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int
                -> Int -> Int -> Int -> Int
                -> Double -> Int -> Int
                -> Int -> Ptr Double -> IO Int

-----------------------------------------------------------------------------

-- | 2D Histogram
histogram2D :: ImageGray -- ^ a
            -> ImageGray -- ^ b
            -> ImageFloat -- ^ histogram
histogram2D (G a) (G b) = unsafePerformIO $ do
    F z <- image (Size 256 256)
    set 0.0 (vroi z) (F z)
    c_histogram2D (ptr a) (step a) (ptr b) (step b) (ptr z) (step z)
               (r1 (vroi a)) (r2 (vroi a)) (c1 (vroi a)) (c2 (vroi a))
               // checkIPP "c_histogram2D" [a,b]
    return (F z)


foreign import ccall "Simple/simple.h histogram2D"
     c_histogram2D :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int
                   -> Int -> Int -> Int -> Int
                   -> IO Int

----------------------------------------------------------------------

-- | LookUp pixel table
lookup2D :: ImageGray -- ^ a
         -> ImageGray -- ^ b
         -> ImageFloat -- ^ 256x256 table
         -> ImageFloat
lookup2D (G a) (G b) (F h) = unsafePerformIO $ do
    F z <- modifyROI (const (vroi a)) `fmap` image (isize a)
    c_lookup2D (ptr a) (step a) (ptr b) (step b) (ptr h) (step h) (ptr z) (step z)
               (r1 (vroi a)) (r2 (vroi a)) (c1 (vroi a)) (c2 (vroi a))
               // checkIPP "c_lookup2D" [a,b]
    return (F z)


foreign import ccall "Simple/simple.h lookup2D"
     c_lookup2D :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int
                   -> Int -> Int -> Int -> Int
                   -> IO Int


{-# OPTIONS -fffi #-}

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
import ImagProc.Ipp.AdHoc(set8u)
import Foreign
import Foreign.C.Types

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
     lbp8u :: Int -> Ptr () -> Int -> Int -> Int -> Int -> Int -> Ptr CInt -> IO Int

-----------------------------------------------------------------------------

-- | to do
hsvCodeTest :: Int -> Int -> Int -> ImageRGB -> IO ()
hsvCodeTest b g w (C im) = do
    hsvcodeTest b g w (ptr im) (step im) (r1 (vroi im)) (r2 (vroi im)) (c1 (vroi im)) (c2 (vroi im))
        // checkIPP "hsvcodeTest" [im]

-- | to do
hsvCode :: Int -> Int -> Int -> ImageRGB -> ImageGray
hsvCode b g w (C im) = unsafePerformIO $ do
    G r <- image (isize im)
    set8u 0 (theROI (G r)) (G r)
    hsvcode b g w
            (ptr im) (step im)
            (ptr r) (step r)
            (r1 (vroi im)) (r2 (vroi im)) (c1 (vroi im)) (c2 (vroi im))
            // checkIPP "hsvcode" [im]
    return $ modifyROI (const (vroi im)) (G r)

foreign import ccall "Simple/simple.h hsvcodeTest"
     hsvcodeTest :: Int -> Int -> Int -> Ptr () -> Int -> Int -> Int -> Int -> Int -> IO Int
foreign import ccall "Simple/simple.h hsvcode"
     hsvcode :: Int -> Int -> Int -> Ptr () -> Int -> Ptr () -> Int -> Int -> Int -> Int -> Int -> IO Int

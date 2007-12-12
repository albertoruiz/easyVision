{-# OPTIONS_GHC -fglasgow-exts #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Generic
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

-}
-----------------------------------------------------------------------------

module ImagProc.Generic (
  GImg(..)
, blockImage
, warp, warpOn
, constImage
, Channels(..), channels, float
)
where

import ImagProc.Ipp.Core
import ImagProc.ImageProcessing
import Foreign.C.Types(CUChar)
import Foreign hiding (shift)
import Numeric.LinearAlgebra
import Vision

class Image image => GImg pixel image | pixel -> image, image -> pixel where
    set :: pixel -> ROI -> image -> IO ()
    copy :: image -> ROI -> image -> ROI -> IO ()
    -- | Resizes the roi of a given image.
    resize :: Size -> image -> image
    warpOnG :: [[Double]] -> image -> image -> IO ()
    fromYUV :: ImageYUV -> image
    toYUV :: image -> ImageYUV

instance GImg CUChar ImageGray where
    set = set8u
    copy = copyROI8u'
    resize sz = unsafePerformIO . resize8u sz
    warpOnG = warpOn8u
    fromYUV = unsafePerformIO . yuvToGray
    toYUV = unsafePerformIO . grayToYUV

instance GImg Float ImageFloat where
    set = set32f
    copy = copyROI32f'
    resize sz = unsafePerformIO . resize32f sz
    warpOnG = warpOn32f
    fromYUV i = unsafePerformIO $ yuvToGray i >>= scale8u32f 0 1
    toYUV i = unsafePerformIO $ scale32f8u 0 1 i >>= grayToYUV

instance GImg (CUChar,CUChar,CUChar) ImageRGB where
    set (r,g,b) = set8u3 r g b
    copy = copyROI8u3'
    resize sz = unsafePerformIO . resize8u3 sz
    warpOnG = warpOn8u3
    fromYUV = unsafePerformIO . yuvToRGB
    toYUV = unsafePerformIO . rgbToYUV

---------------------------------------------------------

-- | joins images
blockImage :: GImg p img  => [[img]] -> img
blockImage = columnImage . map rowImage

--rowImage :: [ImageGray] -> ImageGray
rowImage l = unsafePerformIO $ do
    let r = maximum (map (height.size) l)
        c = maximum (map (width.size) l)
        n = length l
    res <- image (Size r (c*n))
    let roi0 = theROI (head l)
        rois = take n $ iterate (shift (0,c)) roi0
        f r i = copy i roi0 res r
    sequence_ $ zipWith f rois l
    return res

--columnImage :: [ImageGray] -> ImageGray
columnImage l = unsafePerformIO $ do
    let r = maximum (map (height.size) l)
        c = maximum (map (width.size) l)
        n = length l
    res <- image (Size (r*n) c)
    let roi0 = theROI (head l)
        rois = take n $ iterate (shift (r,0)) roi0
        f r i = copy i roi0 res r
    sequence_ $ zipWith f rois l
    return res


--------------------------------------------------------------------

adapt dst h src = toLists $ inv (pixelToPointTrans (size dst)) <> h <> pixelToPointTrans (size src)

-- | Apply a homography (defined on normalized points, see 'pixelsToPoints') to an image.
{-
warp :: Size              -- ^ desired size of the result
     -> Matrix Double     -- ^ homography
     -> ImageFloat        -- ^ source image
     -> IO ImageFloat     -- ^ result
-}

warp out s h im = unsafePerformIO $ do
    r <- image s
    set out (theROI r) r
    warpOn h r im
    return r

-- | The same as 'warp', but the result is written over a preexisting image.
{-
warpOn :: Matrix Double   -- ^ homography
       -> ImageFloat      -- ^ destination image
       -> ImageFloat      -- ^ source image
       -> IO ()
-}
warpOn h r im = warpOnG (adapt r h im) r im

inter_NN         =  1 :: Int  
inter_LINEAR     =  2 :: Int  
inter_CUBIC      =  4 :: Int
inter_SUPER      =  8 :: Int
inter_LANCZOS    = 16 :: Int
--inter_SMOOTH_EDGE = (1 << 31) :: Int

---------------------------------------------------------------------------------

-- hmmmmmmmm
constImage val sz = unsafePerformIO $ do
    z <- image sz
    set val (theROI z) z
    return z

---------------------------------------------------------------------------------

data Channels = CHIm
    { yuv  :: ImageYUV
    , gray :: ImageGray
    , rgb  :: ImageRGB
    , rCh  :: ImageGray
    , gCh  :: ImageGray
    , bCh  :: ImageGray
    , hCh  :: ImageGray
    , sCh  :: ImageGray
    }

channels :: ImageYUV -> Channels
channels img = CHIm
    { yuv = img
    , gray = fromYUV img
    , rgb = rgbAux
    , rCh = getChannel 0 rgbAux
    , gCh = getChannel 1 rgbAux
    , bCh = getChannel 2 rgbAux
    , hCh = getChannel 0 hsvAux
    , sCh = getChannel 1 hsvAux
    }
    where rgbAux = fromYUV img
          hsvAux = unsafePerformIO $ rgbToHSV rgbAux

----------------------------------------------------------------------------------

float :: ImageGray -> ImageFloat
float (G im) = unsafePerformIO $ do
    let roi = vroi im
    r <- scale8u32f 0 1 (modifyROI (const (fullroi im)) (G im))
    return $ modifyROI (const roi) r

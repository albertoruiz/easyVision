{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Generic
Copyright   :  (c) Alberto Ruiz 2007-10
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module ImagProc.Generic (
  GImg(..)
, blockImage
, warp, warpOn, warpOn'
, constImage, cloneClear, clean
, Channels(..), channels, grayscale
, channelsFromRGB
)
where

import ImagProc.Ipp
import Foreign hiding (shift)
import Numeric.LinearAlgebra

class Image image => GImg pixel image | pixel -> image, image -> pixel where
    zeroP :: pixel
    set :: pixel -> ROI -> image -> IO ()
    copy :: image -> ROI -> image -> ROI -> IO ()
    -- | Resizes the roi of a given image.
    resize :: Size -> image -> image
    clone :: image -> IO image
    warpOnG :: [[Double]] -> image -> image -> IO ()
    fromYUV :: ImageYUV -> image
    toYUV :: image -> ImageYUV
    -- | transform an image according to a lookup table (see 'undistortMap')
    remap :: LookupMap -> InterpolationMode -> image -> image
    -- | convencience wrapper of undistortRadial for diag f f 1 cameras
    uradial :: Float -- ^ f parameter in normalized coordinates (e.g. 2.0)
            -> Float -- ^ k radial distortion (quadratic) parameter
            -> image -> image

----------------------------------

instance GImg CUChar ImageGray where
    zeroP = 0
    set = set8u
    copy = copyROI8u
    resize = resize8u InterpLinear
    clone = ioCopy_8u_C1R id
    warpOnG = warpOn8u
    fromYUV = yuvToGray
    toYUV = grayToYUV . clean
    remap (LookupMap m) = remap8u m
    uradial = uradialG undistortRadial8u

instance GImg Float ImageFloat where
    zeroP = 0
    set = set32f
    copy = copyROI32f
    resize = resize32f InterpLinear
    clone = ioCopy_32f_C1R id
    warpOnG = warpOn32f
    fromYUV = float . yuvToGray
    toYUV = grayToYUV . clean . toGray
    remap (LookupMap m) = remap32f m
    uradial = uradialG undistortRadial32f

instance GImg (CUChar,CUChar,CUChar) ImageRGB where
    zeroP = (0,0,0)
    set (r,g,b) = set8u3 r g b
    copy = copyROI8u3
    resize = resize8u3 InterpLinear
    clone = ioCopy_8u_C3R id
    warpOnG = warpOn8u3
    fromYUV = yuvToRGB
    toYUV = rgbToYUV . clean
    remap (LookupMap m) = remapRGB m
    uradial = uradialG undistortRadialRGB


-- modifies the undefined region of an image.
clearNoROI :: Image a => (ROI -> a -> IO ()) -> a -> IO ()
clearNoROI fun im = mapM_ ((flip fun) im) (invalidROIs im)

--cloneClear' :: Image a => a -> IO ()
cloneClear im = do
    r <- clone im
    clearNoROI (set zeroP) r
    return r

-- | creates a new image with the region outside the roi set to zerop
clean :: (GImg pixel a) => a -> a
clean im | (roiArea . theROI) im == (height.size) im * (width.size) im = im
         | otherwise = unsafePerformIO (cloneClear im)

--------------------------------------------------------------------------

-- | joins images
blockImage :: GImg p img  => [[img]] -> img
blockImage = columnImage . map rowImage

--rowImage :: [ImageGray] -> ImageGray
rowImage l = unsafePerformIO $ do
    let r = maximum (map (height.size) l)
        c = maximum (map (width.size) l)
        n = length l
    res <- image (Size r (c*n))
    let f i k = do copy i roi res (m roi)
                   mapM_ (flip (set zeroP) res) (map m (invalidROIs i))
            where m = shift (0,k*c)
                  roi = theROI i
    sequence_ $ zipWith f l [0..]
    return res

--columnImage :: [ImageGray] -> ImageGray
columnImage l = unsafePerformIO $ do
    let r = maximum (map (height.size) l)
        c = maximum (map (width.size) l)
        n = length l
    res <- image (Size (r*n) c)
    let f i k = do copy i roi res (m roi)
                   mapM_ (flip (set zeroP) res) (map m (invalidROIs i))
            where m = shift (k*r,0)
                  roi = theROI i
    sequence_ $ zipWith f l [0..]
    return res


--------------------------------------------------------------------

adapt dst h src = toLists $ inv (pixelToPointTrans (size dst)) <> h <> pixelToPointTrans (size src)

-- | Apply a homography (defined on normalized points, see 'pixelsToPoints') to an image.
warp :: (GImg pixel img) 
     =>  pixel            -- ^ default value for regions outside the transformed roi
     -> Size              -- ^ desired size of the result
     -> Matrix Double     -- ^ homography
     -> img               -- ^ source image
     -> img               -- ^ result
warp out s h im = unsafePerformIO $ do
    r <- image s
    set out (theROI r) r
    warpOn' h r im
    return r

-- | Destructive version of 'warpOn'
warpOn' :: (GImg pixel img)
        => Matrix Double     -- ^ homography
        -> img               -- ^ destination image
        -> img               -- ^ source image
        -> IO ()
warpOn' h r im = warpOnG (adapt r h im) r im

-- | The same as 'warp', but the source image is warped onto a copy of the destination image.
warpOn :: (GImg pixel img)
     => img               -- ^ destination image
     -> Matrix Double     -- ^ homography
     -> img               -- ^ source image
     -> img               -- ^ result
warpOn base h im = unsafePerformIO $ do
    dest <- clone base
    warpOn' h dest im
    return dest

---------------------------------------------------------------------------------

-- hmmmmmmmm
constImage val sz = unsafePerformIO $ do
    z <- image sz
    set val (theROI z) z
    return z

---------------------------------------------------------------------------------

data Channels = CHIm
    { yuv  :: ImageYUV
    , yCh :: ImageGray
    , uCh  :: ImageGray
    , vCh  :: ImageGray
    , rgb  :: ImageRGB
    , rCh  :: ImageGray
    , gCh  :: ImageGray
    , bCh  :: ImageGray
    , hsv  :: ImageRGB
    , hCh  :: ImageGray
    , sCh  :: ImageGray
    }

grayscale :: Channels -> ImageGray
grayscale = yCh

channels :: ImageYUV -> Channels
channels img = CHIm
    { yuv = img
    , yCh = fromYUV img
    , uCh = u
    , vCh = v
    , rgb = rgbAux
    , rCh = getChannel 0 rgbAux
    , gCh = getChannel 1 rgbAux
    , bCh = getChannel 2 rgbAux
    , hsv = hsvAux
    , hCh = getChannel 0 hsvAux
    , sCh = getChannel 1 hsvAux
    }
    where rgbAux = fromYUV img
          hsvAux = rgbToHSV rgbAux
          (u,v) = yuvToUV img

channelsFromRGB :: ImageRGB -> Channels
channelsFromRGB img = CHIm
    { yuv = yuvAux
    , yCh = fromYUV yuvAux
    , uCh = u
    , vCh = v
    , rgb = img
    , rCh = getChannel 0 img
    , gCh = getChannel 1 img
    , bCh = getChannel 2 img
    , hsv = hsvAux
    , hCh = getChannel 0 hsvAux
    , sCh = getChannel 1 hsvAux
    }
    where yuvAux = toYUV img
          hsvAux = rgbToHSV img
          (u,v) = yuvToUV yuvAux

------------------------------------------------

uradialG gen f k im = gen fp fp (fromIntegral w / 2) (fromIntegral h / 2) k 0 im
        where Size h w = size im
              fp = f * fromIntegral w / 2


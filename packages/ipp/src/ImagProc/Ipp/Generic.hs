{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}


module ImagProc.Ipp.Generic(
    Pix(..),
    warp,
    resizeFull,
    constant,
    blockImage,
    Channels(..),channelsFromRGB, grayscale, grayf
) where

import Image.Devel
import ImagProc.Ipp.Core
import ImagProc.Ipp.AdHoc
import ImagProc.Ipp.Pure
import Numeric.LinearAlgebra(Matrix, toLists, (<>), inv, rows, cols)
import Control.Arrow((&&&))

class Storable p => Pix p
  where
    copy   :: Image p -> [(Image p, Pixel)]         -> Image p
    set    :: Image p -> [(ROI, p)]                 -> Image p
    resize :: Size    -> Image p                    -> Image p
    warpon :: Image p -> [(Matrix Double, Image p)] -> Image p
    crossCorr :: Image p -> Image p -> Image Float
    sqrDist   :: Image p -> Image p -> Image Float
    uradial :: Float -- ^ f parameter in normalized coordinates (e.g. 2.0)
            -> Float -- ^ k radial distortion (quadratic) parameter
            -> Image p -> Image p
    zeroP   :: p

{-
-- | transform an image according to a lookup table (see 'undistortMap')
    remap :: LookupMap -> InterpolationMode -> image -> image
-}

instance Pix Word8
  where
    copy    = layerImages copy8u
    set     = setROIs set8u
    resize  = selresize resize8u resize8uNN
    warpon  = warpong warpon8u
    uradial = uradialG undistortRadial8u
    crossCorr = crossCorr8u
    sqrDist = sqrDist8u
    zeroP   = 0
--    remap (LookupMap m) = remap8u m


instance Pix Float
  where
    copy   = layerImages copy32f
    set    = setROIs set32f
    resize = selresize resize32f resize32fNN
    warpon = warpong warpon32f
    uradial = uradialG undistortRadial32f
    crossCorr = crossCorr32f
    sqrDist = sqrDist32f
    zeroP   = 0
--    remap (LookupMap m) = remap32f m


instance Pix Word24
  where
    copy   = layerImages copy8u3
    set    = setROIs set8u3
    resize = selresize resize8u3 resize8u3NN
    warpon = warpong warpon8u3
    uradial = uradialG undistortRadial8u3
    crossCorr t = r3 . crossCorr8u3 t
    sqrDist t = r3 . sqrDist8u3 t
    zeroP   = Word24 0 0 0
--    remap (LookupMap m) = remapRGB m
    
r3 x = resizeFull (Size r (c `div` 3)) x
  where
    Size r c = size x
    


resizeFull :: Pix p => Size -> Image p -> Image p
resizeFull sz'@(Size h' w') im = unsafePerformIO $ do
    r <- newImage undefined sz'
    return $ setROI newroi $ copy r [(x,Pixel r1' c1')]
 where
    Size h w = size im
    sr@(ROI r1 _ c1 _) = roi im
    Size rh rw = roiSize sr
    fh = g h' / g h :: Double
    fw = g w' / g w :: Double
    rh' = round (g rh*fh)
    rw' = round (g rw*fw)
    r1' = round (g r1*fh)
    c1' = round (g c1*fw)
    x = resize (Size rh' rw') im
    newroi = shift (r1',c1') (roi x)
    g n = fromIntegral n


constant :: Pix p => p -> Size -> Image p
constant v sz = unsafePerformIO $ do
    r <- newImage undefined sz
    return $ set r [(fullROI sz, v)]


warp :: Pix p => p -> Size -> Matrix Double -> Image p -> Image p
warp p sz h im = warpon (constant p sz) [(h,im)]



blockImage :: Pix p => [[Image p]] -> Image p
blockImage = colImage . map rowImage
  where
    rowImage = joinImage (\n r c -> Size r (c*n)) (\_ c k (Pixel r1 c1) -> Pixel r1 (c1+k*c))
    colImage = joinImage (\n r c -> Size (r*n) c) (\r _ k (Pixel r1 c1) -> Pixel (r1+k*r) c1)
    joinImage f g xs = copy base $ zip xs rois
      where
        n = length xs
        r = maximum (map (height.size) xs)
        c = maximum (map (width.size)  xs)
        base = constant zeroP (f n r c)
        rois = zipWith h [0..] (map roi xs)
          where
            h k (topLeft -> p) = g r c k p

--------------------------------------------------------------------------------

data Channels = CHIm
    { yuv  :: ImageYUV
    , yCh  :: Image Word8
    , uCh  :: Image Word8
    , vCh  :: Image Word8
    , rgb  :: Image Word24
    , rCh  :: Image Word8
    , gCh  :: Image Word8
    , bCh  :: Image Word8
    , hsv  :: Image Word24
    , hCh  :: Image Word8
    , sCh  :: Image Word8
    , fCh  :: Image Float
    }

grayscale :: Channels -> Image Word8
grayscale = yCh

grayf :: Channels -> Image Float
grayf = fCh

{-
channels :: ImageYUV -> Channels
channels img = CHIm{..}
  where
    yuv = img
    yCh = fromYUV img
    uCh = u
    vCh = v
    rgb = rgbAux
    rCh = getChannel 0 rgbAux
    gCh = getChannel 1 rgbAux
    bCh = getChannel 2 rgbAux
    hsv = hsvAux
    hCh = getChannel 0 hsvAux
    sCh = getChannel 1 hsvAux
    fCh = float yCh
    rgbAux = fromYUV img
    hsvAux = rgbToHSV rgbAux
    (u,v) = yuvToUV img

-}

channelsFromRGB :: Image Word24 -> Channels
channelsFromRGB img = CHIm{..}
  where
    yuv = yuvAux
    yCh = rgbToGray img
    uCh = u
    vCh = v
    rgb = img
    rCh = getChannel 0 img
    gCh = getChannel 1 img
    bCh = getChannel 2 img
    hsv = hsvAux
    hCh = getChannel 0 hsvAux
    sCh = getChannel 1 hsvAux
    fCh = toFloat yCh
    yuvAux = rgbToYUV img
    hsvAux = rgbToHSV img
    (u,v) = yuvToUV yuvAux


--------------------------------------------------------------------------------


layerImages
    :: (Image p -> Image p -> Pixel -> IO ())
    -> Image p -> [(Image p, Pixel)] -> Image p
layerImages g im ptsIms = unsafePerformIO $ do
    res <- cloneImage im
    mapM_ (f res) ptsIms
    return res
  where
    f res (x, p) = g x res p

setROIs
    :: (p -> ROI -> Image p -> IO ())
    -> Image p -> [(ROI, p)] -> Image p
setROIs g im rps = unsafePerformIO $ do
    res <- cloneImage im
    mapM_ (f res) rps
    return res
  where
    f res (x, p) = g p x res


selresize
  :: (Size -> Image p -> t) -> (Size -> Image p -> t)
  -> Size -> Image p -> t
selresize f1 f2 sz im
    | roiArea sr < 1       = error $ "resize input " ++ show sr
    | r1 == r2 || c1 == c2 = f2 sz im
    | otherwise            = f1 sz im
  where
    sr@(ROI r1 r2 c1 c2) = roi im


warpong
  :: ([[Double]] -> Image p -> Image p -> IO ())
     -> Image p -> [(Matrix Double, Image p)] -> Image p
warpong g im hxs = unsafePerformIO $ do
    res <- cloneImage im
    mapM_ (f res) hxs
    return res
  where
    f res (h,x) | ok = g adapt x res
                | otherwise = error $ "warp homography wrong dimensions " ++ show szh
      where
        adapt = toLists $ inv (pixelToPointTrans (size res)) <> h <> pixelToPointTrans (size x)
        szh = (rows &&& cols) h
        ok = szh == (3,3)

uradialG :: (Float -> Float -> Float -> Float -> Float -> Float -> Image p -> Image p)
         -> Float -> Float -> Image p -> Image p
uradialG gen f k im = gen fp fp (fromIntegral w / 2) (fromIntegral h / 2) k 0 im
        where Size h w = size im
              fp = f * fromIntegral w / 2


{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns #-}

-- U-V color segmentation

import EasyVision
import Graphics.UI.GLUT hiding (minmax,Size,Matrix)
import Control.Arrow


$(autoParam "LikParam" ""
  [( "scale","Float" ,realParam 100 0 200),
   ( "sigma","Float" ,realParam 1 0 5)]
 )


main = run $ camera -- >>= observe "Image" rgb
           >>= selectROI "select ROI" rgb
           ~> (fst &&& hist . uv)
           >>= selectHistogram >>= updateModel
           >>= liks .@. winLikParam
           >>= observe "Likelihood" snd
           ~>  mask
           >>= observe "Mask" shMask
           >>= wcontours snd ~> (fst *** contWhite) >>= shRegions
           >>= timeMonitor

----------------------------------------------------------------------

uv (im, r) = (f *** f) (uCh im, vCh im)
    where f = modifyROI (const (roiDiv 2 r))

hist = norm . uncurry histogram2D

norm img = recip mx .* img
  where (mn,mx) = minmax img

norm' img = recip s .* img
  where s = d2f $ sum32f img
        d2f = fromRational . toRational


lik (x,h) = lookup2D (uCh x) (vCh x) h

liks LikParam {..} (x,h) = scale .* lik (x, gaussS sigma h)

----------------------------------------------------------------------

close = dilate3x3 . erode3x3 
open = erode3x3 . dilate3x3

mask ((im,_),lk) = (im,mk)
  where
    mk = post $ one |-| thresholdVal32f 1 1 IppCmpGreater lk
    one = constImage 1 (size lk)
    post = close . toGray

shMask (im,mk) = float mk |*| float g
  where
    g = resize (size mk) $ grayscale im

----------------------------------------------------------------------

selectHistogram = selectSnd "UV Hist" f where
    f (im,h) = drawImage' (30 .* h)

updateModel = updateMaybeSave "color model" "model" g (constImage (0::Float) (Size 256 256)) (drawImage'.snd)
 where f = (\a b -> 0.5.*(a |+| b))
       g a b = norm $ maxEvery a b
       h a b = a |+| b

----------------------------------------------------------------------

instance Show ImageFloat where
   show = show . img2mat

instance Read ImageFloat where
   readsPrec _ s = [(mat2img $ read s,"")]

----------------------------------------------------------------------

shRegions = monitor "Regions" (mpSize 20) sh where
  sh (im,cs) = do
      drawImage (rgb im)
      pointCoordinates (mpSize 20)
      lineWidth $= 3
      setColor 0 0 1
      mapM_ shcont cs
      
      
shcont (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c
shcont (Open c) = do
    renderPrimitive LineStrip $ mapM_ vertex c


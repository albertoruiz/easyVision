{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns #-}

-- U-V color segmentation

import EasyVision
import Graphics.UI.GLUT hiding (minmax,Size,Matrix)
import Control.Arrow


$(autoParam "LikParam" ""
  [( "scale","Float" ,realParam 100 0 200)]
 )


main = run $ camera -- >>= observe "Image" rgb
           >>= selectROI "select ROI" rgb
           ~> (fst &&& hist . uv)
           >>= selectHistogram >>= updateModel
           >>= liks .@. winLikParam
           >>= observe "Likelihood" snd
           >>= observe "Mask" mask
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

liks LikParam {..} (x,h) = scale .* lik (x,h)

----------------------------------------------------------------------

mask ((im,h),lk) = m |*| g
  where
    m = id $ one |-| thresholdVal32f 1 1 IppCmpGreater lk
    g = resize sz2 $ float (gray im)
    one = constImage 1 sz2
    sz2 = Size (div r 2) (div c 2) where Size r c = size (gray im) 
    close = dilate3x3 . erode3x3 
    open = erode3x3 . dilate3x3
    post = float . close . toGray

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


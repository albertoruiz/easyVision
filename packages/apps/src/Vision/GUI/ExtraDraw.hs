{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
{- |
Module      :  Vision.GUI.ExtraDraw
Copyright   :  (c) Alberto Ruiz 2006-12
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Drawing utilities.

-}
-----------------------------------------------------------------------------

module Vision.GUI.ExtraDraw
( extractSquare
) where

import Vision.GUI.Simple
import Image.Core
import ImagProc
import Numeric.LinearAlgebra


{-# DEPRECATED extractSquare "use new drawing camera tools" #-}
extractSquare :: Int -> ImageFloat -> ImageFloat
extractSquare sz (F im) = resize (Size sz sz) (F im {vroi = roi}) where
    w = width $ isize im
    h = height $ isize im
    d = w-h
    dm = d `quot` 2
    roi = (vroi im) {c1=dm-1,c2= dm+h}

---------------------------------------------------------

instance Renderable ImageFloat where
    renderIn w = renderIn w . toGray

instance Renderable ImageYUV where
    renderIn w = renderIn w . yuvToRGB

instance Renderable ImageYCbCr where
    -- renderIn w (Y422 im) = renderImageIn w im
    renderIn w = renderIn w . yCbCrToRGB


instance Renderable Channels
  where
    renderIn q CHIm{..} = do
        renderIn q things
        render (color orange $ text (Point (0.5) (0.5)) "grayscale")
        render (color red    $ text (Point (-0.5) (0.5)) "red channel")
        render (color green  $ text (Point (0.5) (-0.5)) "green channel")
        render (color blue   $ text (Point (-0.5) (-0.5)) "blue channel")
        render (color orange $ histn yCh)
        render (color red    $ histn rCh)
        render (color green  $ histn gCh)
        render (color blue   $ histn bCh)
      where
        things = (blockImage . map (map f)) [[yCh, rCh],[gCh,bCh]]
        f = resize (Size (h `div`2) (w `div` 2))
        Size h w = size (yCh)
        histn c = hist / scalar (maxElement hist) - 0.6
          where
            hist = fromList $ histogramN [0..256] c


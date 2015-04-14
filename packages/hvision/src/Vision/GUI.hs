{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}


-----------------------------------------------------------------------------
{- |
Module      :  Vision.GUI
Copyright   :  (c) Alberto Ruiz 2007-13
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module Vision.GUI (
    module Vision.GUI.Simple,
    Vision.GUI.camera, Vision.GUI.run,
    module Vision.Apps.Show
) where

import Vision.GUI.Simple hiding (camera,run)
import qualified Vision.GUI.Simple as S
import Image
import Image.Capture(gcam)
import Image.Processing
import Numeric.LinearAlgebra.HMatrix
import Vision.Apps.Show
import Util.Options


instance Renderable (Image Float) where
    renderIn w = renderIn w . toGray




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
        Size h w = Image.size (yCh)
        histn c = hist / scalar (maxElement hist) - 0.6
          where
            hist = fromList $ histogramN [0..256] c


fillChannels :: Generator Channels -> Generator Channels
fillChannels = fmap (fmap (fmap (channelsFromRGB.rgb)))

run :: ITrans Channels b -> IO ()
run t = ippSetNumThreads 1 >> runT_ Vision.GUI.camera (t >>> optDo "--freq" freqMonitor)

camera :: Generator Channels
camera = do
    f <- hasValue "--photos"
    g <- hasValue "--photosmp"
    h <- hasValue "--sphotos"
    if f || g || h
      then fillChannels S.camera
      else cameraV
      
cameraV :: Generator Channels
cameraV = fmap (fmap (fmap channelsFromYUYV)) gcam


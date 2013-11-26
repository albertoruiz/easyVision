{-# LANGUAGE FlexibleInstances #-}


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
    Vision.GUI.camera, Vision.GUI.run
) where

import Vision.GUI.Simple hiding (camera,run)
import Vision.GUI.Util
import Image.Convert
import Image.Processing
import Image.Core

instance Renderable (Image Float) where
    renderIn w = renderIn w . toGray


{-

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
-}

camera :: Generator Channels
camera = toRGB Vision.GUI.Util.camera

toRGB :: Generator ImageRGB -> Generator Channels
toRGB = fmap (fmap (fmap channelsFromRGB))

run :: ITrans Channels b -> IO ()
run t = ippSetNumThreads 1 >> runT_ Vision.GUI.camera (t >>> optDo "--freq" freqMonitor)


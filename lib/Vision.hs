{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  Vision
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Computer vision, pattern recognition and image processing.

-}
-----------------------------------------------------------------------------

module Vision (
    module Vision.Stat,
    module Vision.Classifier,
    module Vision.Geometry,
    module Vision.Autofrontal
) where

import Vision.Stat
import Vision.Classifier
import Vision.Geometry
import Vision.Autofrontal
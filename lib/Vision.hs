{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  Vision
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Some algorithms for geometric computer vision.

-}
-----------------------------------------------------------------------------

module Vision (
    module Vision.Geometry,
    module Vision.Autofrontal
) where

import Vision.Geometry
import Vision.Autofrontal
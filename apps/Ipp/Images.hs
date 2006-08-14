{-# OPTIONS -fglasgow-exts #-}

-----------------------------------------------------------------------------
{- |
Module      :  Ipp.Images
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Type safe interface to the IPP images.

-}
-----------------------------------------------------------------------------

module Ipp.Images
( -- * Image types
  Image (size)
, ImageRGB
, ImageGray
, ImageFloat
  -- * Image coordinates
, Pixel (..)
, Point (..)
, pixelsToPoints
, val32f
) where

import GSL hiding (size)
import Vision
import Ipp.Core hiding (width, height, img)
import Ipp.Camera
import qualified Ipp.Core as Core
import Foreign


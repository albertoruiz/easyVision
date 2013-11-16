-----------------------------------------------------------------------------
{- |
Module      :  Images.Types
Copyright   :  (c) Alberto Ruiz 2006-10
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional


Type safe interface to the IPP images.

These are the only definitions required by the \"end user\". Lower level programming requires "Ipp.Core".

-}
-----------------------------------------------------------------------------

module Image.Types
( -- * Image types
  Image (..)
, ImageRGB
, ImageGray
, ImageFloat
, ImageYUV
, module Image.Base
, module Image.ROI
) where

import Image.Core (
     Image(..)
   , ImageRGB
   , ImageGray
   , ImageFloat
   , ImageYUV )

import Image.Base
import Image.ROI


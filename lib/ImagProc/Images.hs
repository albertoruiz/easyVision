{-# OPTIONS -fglasgow-exts #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Images
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Type safe interface to the IPP images.

These are the only definitions required by the \"end user\". Lower level programming requires "Ipp.Core".

-}
-----------------------------------------------------------------------------

module ImagProc.Images
( -- * Image types
  Image (..)
, ROI(..)
, ImageRGB
, ImageGray
, ImageFloat
, ImageYUV
  -- * Image coordinates
, Size (..)
, Pixel (..)
, Point (..)
, Segment (..), segmentLength
, pixelsToPoints
--, val32f
) where

import ImagProc.Ipp.Core (
     Image(..)
   , ROI(..)
   , ImageRGB
   , ImageGray
   , ImageFloat
   , ImageYUV
   , Pixel(..)
   , Point(..)
   , Segment(..), segmentLength
   , Size(..)
   , pixelsToPoints
 )


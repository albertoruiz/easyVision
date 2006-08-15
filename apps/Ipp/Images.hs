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

These are the only definitions required by the \"end user\". Lower level programming requires "Ipp.Core".

-}
-----------------------------------------------------------------------------

module Ipp.Images
( -- * Image types
  Image (size)
, ImageRGB
, ImageGray
, ImageFloat
  -- * Image coordinates
, Size (..)
, Pixel (..)
, Point (..)
, pixelsToPoints
--, val32f
) where

import Ipp.Core (
     Image(..)
   , ImageRGB
   , ImageGray
   , ImageFloat
   , Pixel(..)
   , Point(..)
   , Size(..)
   , pixelsToPoints
 )


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
, ImageRGB
, ImageGray
, ImageFloat
, ImageYUV
, module ImagProc.Base
, module ImagProc.ROI
) where

import ImagProc.Ipp.Core (
     Image(..)
   , ImageRGB
   , ImageGray
   , ImageFloat
   , ImageYUV )

import ImagProc.Base
import ImagProc.ROI


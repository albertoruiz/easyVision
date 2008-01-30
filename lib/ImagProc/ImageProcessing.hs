{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.ImageProcessing
Copyright   :  (c) Alberto Ruiz 2006/8
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  requires IPP

Image processing functions as pure functions

-}
-----------------------------------------------------------------------------

module ImagProc.ImageProcessing (
    module ImagProc.Ipp.Manual,
    module ImagProc.Ipp.Auto,
    module ImagProc.Pure,
    module ImagProc.Generic
) where

import ImagProc.Ipp.Manual
import ImagProc.Ipp.Auto
import ImagProc.Pure
import ImagProc.Generic

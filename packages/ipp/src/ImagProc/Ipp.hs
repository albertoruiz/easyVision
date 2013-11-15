{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Ipp
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Experimental interface to Intel's IPP image processing libraries

-}
-----------------------------------------------------------------------------

module ImagProc.Ipp (
    module Image.Core,
    module Image.Convert,
    module ImagProc.Ipp.Wrappers,
    module ImagProc.Ipp.AdHoc,
    module ImagProc.Ipp.Adapt,
    module ImagProc.Ipp.Auto,
    module ImagProc.Ipp.Pure
) where

import Image.Core
import Image.Convert
import ImagProc.Ipp.Wrappers
import ImagProc.Ipp.AdHoc
import ImagProc.Ipp.Adapt
import ImagProc.Ipp.Auto
import ImagProc.Ipp.Pure

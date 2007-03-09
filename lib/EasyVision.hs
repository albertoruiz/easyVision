{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  EasyVision
Copyright   :  (c) Alberto Ruiz 2006,2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  Requires HOpenGL and uses MPlayer

A minimalist version of EasyVision, a system for fast prototyping of
computer vision and image processing applications.

A few example programs can be found in the compvis folder.

-}
-----------------------------------------------------------------------------

module EasyVision (
    module EasyVision.GUI,
    module EasyVision.Parameters,
    module EasyVision.Combinators,
    module ImagProc
) where

import EasyVision.GUI
import EasyVision.Parameters
import EasyVision.Combinators
import ImagProc
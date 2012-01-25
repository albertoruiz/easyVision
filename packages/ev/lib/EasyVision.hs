{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  EasyVision
Copyright   :  (c) Alberto Ruiz 2006,2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  Requires HOpenGL and uses MPlayer

A system for fast prototyping of computer vision and image processing applications.

A few example programs can be found in the compvis folder.

-}
-----------------------------------------------------------------------------

module EasyVision (
    module EasyVision.GUI,
    module EasyVision.MiniApps,
    module ImagProc,
    module Features,
    module Contours,
    module ImagProc.Camera,
    module Util.LazyIO
) where

import EasyVision.GUI
import EasyVision.MiniApps
import ImagProc
import Features
import Contours
import ImagProc.Camera
import Util.LazyIO

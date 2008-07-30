{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  Features
Copyright   :  (c) Alberto Ruiz 2008
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional

Several kind of geometric primitives

-}
-----------------------------------------------------------------------------

module Features (
    module Features.Polyline,
    module ImagProc.C.Segments,
    module Features.InterestPoints,
    module Features.Matching
) where

import Features.Matching
import ImagProc.C.Segments
import Features.Polyline
import Features.InterestPoints

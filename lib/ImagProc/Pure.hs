-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Pure
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Some image processing routines expressed as pure functions, instead the IO approach in ImagProc.ImageProcessing. Here we must decide what to do outside the roi to preserve referential transparency.

-}
-----------------------------------------------------------------------------

module ImagProc.Pure (
    (.*),
    (|+|),(|-|),(|*|)
)
where

import ImagProc.Ipp.Core
import ImagProc.ImageProcessing

infixl 7  |*|, .*
infixl 6  |+|, |-|

-- | image scaling, zero outside the roi
(.*) :: Float -> ImageFloat -> ImageFloat
v .* im = purifyWith (set32f 0) (scale32f v im)

-- | image arithmetic, pixel by pixel, zero outside the roi
(|+|),(|-|),(|*|) :: ImageFloat -> ImageFloat -> ImageFloat
a |+| b = purifyWith (set32f 0) (a `add32f` b)
a |-| b = purifyWith (set32f 0) (a `sub32f` b)
a |*| b = purifyWith (set32f 0) (a `mul32f` b)
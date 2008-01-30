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
    pure,
    (.*),
    (|+|),(|-|),(|*|),
    andI,orI,notI
)
where

import ImagProc.Ipp
import ImagProc.Generic

pure f = purifyWith (set zeroP) f

infixl 7  |*|, .*
infixl 6  |+|, |-|

-- | image scaling, zero outside the roi
(.*) :: Float -> ImageFloat -> ImageFloat
v .* im = pure (scale32f v im)

-- | image arithmetic, pixel by pixel, zero outside the roi
(|+|),(|-|),(|*|) :: ImageFloat -> ImageFloat -> ImageFloat
a |+| b = pure (a `add32f` b)
a |-| b = pure (a `sub32f` b)
a |*| b = pure (a `mul32f` b)

-- | image logic, pixel by pixel, false outside the roi
andI, orI :: ImageGray -> ImageGray -> ImageGray
andI a b = pure (a `and8u` b)
orI  a b = pure (a `or8u`  b)

notI :: ImageGray -> ImageGray
notI a = pure (not8u a)

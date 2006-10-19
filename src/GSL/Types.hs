{-# OPTIONS #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GSL.Types
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL-style
-- 
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- basic definitions useful for both vectors and matrices
--
-----------------------------------------------------------------------------

-- #hide
module GSL.Types(
    Vector(..), Matrix(..),
    size, rows, cols,
    module Complex
) where

import Foreign
import Complex

----------------------------------------------------------------------
instance (Storable a, RealFloat a) => Storable (Complex a) where    --
    alignment x = alignment (realPart x)                            --
    sizeOf x    = 2 * sizeOf (realPart x)                           --
    peek p = do                                                     --
        [re,im] <- peekArray 2 (castPtr p)                          --
        return (re :+ im)                                           --
    poke p (a :+ b) = pokeArray (castPtr p) [a,b]                   --
----------------------------------------------------------------------

-- | 1D array
data Vector t = V Int (ForeignPtr t)

-- | Number of elements of a 'Vector'.
size :: Vector t -> Int
size (V n _) = n

-- | 2D array
data Matrix t = M Int Int (ForeignPtr t)

-- | Number of rows of a 'Matrix'.
rows :: Matrix t -> Int
rows (M r _ _) = r
-- | Number of columns of a 'Matrix'.
cols :: Matrix t -> Int
cols (M _ c _) = c

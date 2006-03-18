{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSL.Util
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses -fffi and -fglasgow-exts

Utility functions easily derivable from the basic ones in "GSL".

-}
-----------------------------------------------------------------------------

module GSL.Util (
    -- * Easy creation of vectos and matrices
    realVector, realMatrix, complexVector, complexMatrix, 
    -- * Matrix IO
    fromFile, gslReadMatrix,
    -- * Special matrices
    i, norm, hilb, ident, constant, 
    -- * Additional matrix manipulation
    extractRows, fromRows, toRows, fromCols, toCols, flipud, fliprl, 
    -- * Other
    (//), disp,  sumCols, outer
) where

import GSL
import GSL.Base
import GSL.Interface
import GSL.Derived
import GSL.Wrappers
import Foreign

-- | creates a real vector from a list. Useful in some situations to avoid type annotations.
realVector :: [Double] -> V
realVector = fromList

-- | creates a complex vector from a list. Useful in some situations to avoid type annotations.
complexVector :: [Complex Double] -> CV
complexVector = fromList

-- | creates a real vector from a list of lists. Useful in some situations to avoid type annotations.
realMatrix :: [[Double]] -> M
realMatrix = fromLists

-- | creates a complex vector from a list. Useful in some situations to avoid type annotations.
complexMatrix :: [[Complex Double]] -> CM
complexMatrix = fromLists

{- | postfix function application with low precedence (as in Mathematica)

> > hilb 10 // eig // fst // toList // maximum
> 1.7519196702651774

-}
(//) :: a -> (a -> b) -> b
infixl 1 //
x // f = f x
------------------------------------------    

-- | reverse rows 
flipud :: Storable t => Matrix t -> Matrix t
flipud m = fromRows . reverse . toRows $ m

-- | reverse columns
fliprl :: (Storable t, Trans t) => Matrix t -> Matrix t
fliprl m = fromCols . reverse . toCols $ m   

{- | display with n digits after the decimal point.
-}
disp :: (Disp a) => Int -> a -> IO ()
disp n = putStrLn . format n

--------------------------------------------

-- | sum of columns of a matrix.
sumCols :: (Mul V (Matrix t) (Vector t)) => Matrix t -> Vector t
sumCols m = constant 1 (rows m) <> m

-- | outer product of two vectors.
outer :: (Mul (Matrix a) (Matrix b) (Matrix r)) => Vector a -> Vector b -> Matrix r
outer u v = reshape 1 u <> reshape (size v) v



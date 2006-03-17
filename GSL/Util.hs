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
    vector, matrix, realVector, realMatrix,
    i, norm, hilb, ident, constant, extractRows, fromRows, toRows, fromCols, toCols,
    (//), disp, flipud, fliprl, fromFile, sumCols, outer
) where

import GSL
import GSL.Base
import GSL.Interface
import GSL.Derived
import Foreign

realVector :: [Double] -> V
realVector = fromList

vector :: [Complex Double] -> CV
vector = fromList

realMatrix :: [[Double]] -> M
realMatrix = fromLists

matrix :: [[Complex Double]] -> CM
matrix = fromLists

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

{- | display with n digits after the decimal point

-}
disp :: (Disp a) => Int -> a -> IO ()
disp n = putStrLn . format n

--------------------------------------------

-- | sum of columns of a matrix
sumCols :: (Mul V (Matrix t) (Vector t)) => Matrix t -> Vector t
sumCols m = constant 1 (rows m) <> m

-- | outer product of two vectors
outer :: (Mul (Matrix a) (Matrix b) (Matrix r)) => Vector a -> Vector b -> Matrix r
outer u v = reshape 1 u <> reshape (size v) v

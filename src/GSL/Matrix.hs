{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSL.Matrix
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Basic operations on vectors and matrices

(In construction...)

-}
-----------------------------------------------------------------------------
module GSL.Matrix (
    -- * Base types
    -- | This library works with ...
    Field, Container,
    -- * Vectors
    Vector, size, (@>),
    fromList, toList,
    fromArray1D, toArray1D,
    -- * Matrices
    Matrix, rows, cols, (@@>),
    fromLists, toLists,
    fromArray2D, toArray2D,
    -- * Matrix manipulation
    subVector, join,
    subMatrix, fromBlocks, (<|>), (<->),
    fromRows, toRows, fromColumns, toColumns,
    takeRows, takeColumns, dropRows, dropColumns,
    reshape, flatten,
    asRow, asColumn,
    toComplex, fromComplex, complex,
    flipud, fliprl, extractRows, triang,
    gmap, gzip,
    -- * Utilities
    diag, takeDiag, conj,
    trans, ident,
    constant, linspace, vectorMax, vectorMin, vectorMaxIndex, vectorMinIndex,
    format, disp, dispR, dispC, fromFile

) where

import GSL.Types
import GSL.Wrappers
import GSL.Common
import Numeric(showGFloat)
import Data.List(transpose,intersperse)

-- shows a Double with n digits after the decimal point    
shf :: (RealFloat a) => Int -> a -> String     
shf dec n | abs n < 1e-10 = "0."
          | abs (n - (fromIntegral.round $ n)) < 1e-10 = show (round n) ++"."
          | otherwise = showGFloat (Just dec) n ""    
-- shows a Complex Double as a pair, with n digits after the decimal point    
shfc n z@ (a:+b) 
    | magnitude z <1e-10 = "0."
    | abs b < 1e-10 = shf n a
    | abs a < 1e-10 = shf n b ++"i"
    | b > 0         = shf n a ++"+"++shf n b ++"i"
    | otherwise     = shf n a ++shf n b ++"i"         

dsp :: String -> [[String]] -> String
dsp sep as = unlines . map unwords' $ transpose mtp where 
    mt = transpose as
    longs = map (maximum . map length) mt
    mtp = zipWith (\a b -> map (pad a) b) longs mt
    pad n str = replicate (n - length str) '_' ++ str
    unwords' = concat . intersperse sep

format :: (Field t) => String -> (t -> String) -> Matrix t -> String
format sep f m = dsp sep . map (map f) . toLists $ m

disp m f = putStrLn $ "matrix ("++show (rows m) ++"x"++ show (cols m) ++")\n"++format "_|_" f m

dispR d m = disp m (shf d)
dispC d m = disp m (shfc d)
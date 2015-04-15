{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
{- |
Module      :  Util.Kron
Copyright   :  (c) Alberto Ruiz 2015
License     :  BSD3
Maintainer  :  Alberto Ruiz
Stability   :  provisional

Tools for the Kronecker product

(see A. Fusiello, A matter of notation: Several uses of the Kronecker product in
3d computer vision, Pattern Recognition Letters 28 (15) (2007) 2127-2132)

@`vec` (a \<> x \<> b) == (tr b `kronecker` a) \<> 'vec' x@

-}
-----------------------------------------------------------------------------

module Util.Kron(
    vec,
    vech,
    dup,
    vtrans,
    kroneckerTest
) where

import Numeric.LinearAlgebra

vec :: Numeric t => Matrix t -> Vector t
-- ^ stacking of columns
vec = flatten . tr


vech :: Numeric t => Matrix t -> Vector t
-- ^ half-vectorization (of the lower triangular part)
vech m = vjoin . zipWith f [0..] . toColumns $ m
  where
    f k v = subVector k (size v - k) v


dup :: (Numeric t, Num (Vector t)) => Int -> Matrix t
-- ^ duplication matrix (@'dup' k \<> 'vech' m == 'vec' m@, for symmetric m of dim k)
dup k = tr $ fromRows $ map f es
  where
    rs = zip [0..] (toRows (ident (k^(2::Int))))
    es = [(i,j) | j <- [0..k-1], i <- [0..k-1], i>=j ]
    f (i,j) | i == j = g (k*j + i)
            | otherwise = g (k*j + i) + g (k*i + j)
    g j = v
      where
        Just v = lookup j rs


vtrans :: Element t => Int -> Matrix t -> Matrix t
-- ^ generalized \"vector\" transposition: @'vtrans' 1 == 'tr'@, and @'vtrans' ('rows' m) m == 'asColumn' ('vec' m)@
vtrans p m | r == 0 = fromBlocks . map (map asColumn . takesV (replicate q p)) . toColumns $ m
           | otherwise = error $ "vtrans " ++ show p ++ " of matrix with " ++ show (rows m) ++ " rows"
  where
    (q,r) = divMod (rows m) p

--------------------------------------------------------------------------------

kroneckerTest :: Bool
kroneckerTest = ok
  where
    a,x,b :: Matrix Double
    a = (3><4) [1..]
    x = (4><2) [3,5..]
    b = (2><5) [0,5..]
    v1 = vec (a <> x <> b)
    v2 = (tr b `kronecker` a) #> vec x
    s = tr b <> b
    v3 = vec s
    v4 = (dup 5 :: Matrix Double) #> vech s
    ok = v1 == v2 && v3 == v4
      && vtrans 1 a == tr a
      && vtrans (rows a) a == asColumn (vec a)


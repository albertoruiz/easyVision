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
    subMatrix, fromBlocks,
    fromRows, toRows, fromColumns, toColumns,
    takeRows, takeColumns, dropRows, dropColumns,
    reshape, flatten,
    asRow, asColumn,
    toComplex, fromComplex, complex,
    flipud, fliprl, extractRows, triang,
    -- * Utilities
    diag, takeDiag, conj,
    trans, ident,
    constant, linspace, vectorMax, vectorMin, vectorMaxIndex, vectorMinIndex

) where

import GSL.Types
import GSL.Wrappers
import GSL.Common
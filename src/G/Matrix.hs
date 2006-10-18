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
module G.Matrix (
    -- * Base types
    -- | This library works with ...
    Field, Container,
    -- * Vectors
    GSLVector, size, (@>),
    fromList, toList,
    fromArray1D, toArray1D,
    -- * Matrices
    GSLMatrix, rows, cols, (@@>),
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
    constant, linspace

) where

import G.Types
import G.Wrappers
import G.Common
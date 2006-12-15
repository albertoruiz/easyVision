{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSL.LinearAlgebra.Vector
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Vectors

(In construction...)

-}
-----------------------------------------------------------------------------
module GSL.LinearAlgebra.Vector (
    -- * The vector type
    Vector, size, (@>),
    fromList, toList,
    fromArray1D, toArray1D,
    -- * Vector manipulation
    subVector, join,
    constant, linspace, vectorMax, vectorMin, vectorMaxIndex, vectorMinIndex,
) where

import GSL.Types
import GSL.Wrappers
import GSL.Common
import Numeric(showGFloat)
import Data.List(transpose,intersperse)

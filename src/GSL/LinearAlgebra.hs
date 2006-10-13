{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSL.LinearAlgebra
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Some linear algebra algorithms working on UArrays, implemented by means of the
GSL or Lapack, or directly 

(In construction...)

-}
-----------------------------------------------------------------------------
module GSL.LinearAlgebra (
    module GSL.LinearAlgebra.Matrix,
    module GSL.LinearAlgebra.Unboxed
) where

import GSL.LinearAlgebra.Matrix
import GSL.LinearAlgebra.Unboxed



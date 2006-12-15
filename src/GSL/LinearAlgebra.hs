{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSL.LinearAlgebra
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Some linear algebra algorithms, implemented by means of BLAS, LAPACK or GSL.

(In construction...)

-}
-----------------------------------------------------------------------------
module GSL.LinearAlgebra (
    -- * Data types
    Container, Field,
    module GSL.LinearAlgebra.Vector,
    module GSL.LinearAlgebra.Matrix,
    -- * Algorithms
    module GSL.LinearAlgebra.Algorithms,
    -- * Utilities
    module Complex,
    i, eps,
    toComplex, fromComplex, complex, conj,
    gmap, gzip,
) where

import Complex
import GSL.LinearAlgebra.Vector
import GSL.LinearAlgebra.Matrix
import GSL.LinearAlgebra.Algorithms
import GSL.Common
import GSL.Instances

------------------------------------------------------------------------------




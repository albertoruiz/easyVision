-----------------------------------------------------------------------------
{- |
Module      :  GSL
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses -fffi and -fglasgow-exts

Wrappers for some useful functions of the GNU Scientific Library (<http://www.gnu.org/software/gsl>).
 
This module provides a simple functional interface to standard matrix computations (eigensystems, singular values, pseudoinverses, etc.) and other numeric algorithms.

More information can be found at <http://dis.um.es/~alberto/GSLHaskell>

-}
-----------------------------------------------------------------------------

module GSL (
    -- * Data types
    -- | This library works with four data types: real vectors ('V'), complex vectors ('CV'), real matrices ('M') and complex matrices ('CM'). They are statically different, inmutable, and with automatic garbage collection. They are implemented as Foreign pointers to C-style arrays of double with dimension information, corresponding to the GSL vectors and matrices. 
    
    -- ** Vectors
    V, CV, fromList, toList,
    -- ** Matrices 
    M, CM, fromLists, toLists, 
    -- * Basic Linear Algebra
    (<>), (|+|), (|-|),  (<\>), det, pnorm, (.*),
    -- * Matrix factorizations
    eig, svd, qr, chol, lu,
    -- * Fourier Transform
    fft, ifft,
    -- * Numerical functions
    -- ** Polynomials
    polySolve, 
    -- ** Integration
    integrateQNG, integrateQAGS,
    -- * Matrix manipulation
    size, rows, cols, diag, trans, conj, subVector, subMatrix,  
    flatten, reshape, fromBlocks, format,
    
    module Complex
) where

import GSL.Base
import GSL.Wrappers
import GSL.Derived
import GSL.Interface
import Complex



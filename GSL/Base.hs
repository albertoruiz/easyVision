-----------------------------------------------------------------------------
{- |
Module      :  GSL
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses -fffi and -fglasgow-exts

Interface to the GSL and basic infrastructure for manipulation of vectors and matrices.

-}
-----------------------------------------------------------------------------

module GSL.Base (
    -- * Data types
    -- | The library works with four data types: real vectors ('V'), complex vectors ('CV'), real matrices ('M') and complex matrices ('CM'). They are statically different, inmutable, and with automatic garbage collection. They are implemented as Foreign pointers to C-style arrays of double with dimension information, corresponding to the GSL vectors and matrices. These types are internally similar to StorableArray's, but they are inmutable and can be used outside the IO monad.)
    
    -- ** Vectors
    -- | (TO DO: sort description, read, show, and numeric instances)
    V, CV, fromList, toList,
    -- ** Matrices 
    -- | (TO DO: sort description, read, show, and numeric instances)
    M, CM, fromLists, toLists, 
    -- * Basic Linear Algebra
    (<>), (<\>), det, pnorm,
    -- * Matrix factorizations
    eig, svd, qr, chol, lu,
    -- * Fourier Transform
    fft, ifft,
    -- * Numerical functions
    -- ** Polynomials
    polySolve, 
    -- ** Integration
    integrateQNG, integrateQAGS,
    -- ** Differentiation
    -- | From the GSL manual, <http://www.gnu.org/software/gsl/manual/gsl-ref_toc.html#TOC407>: \"The functions described in this chapter compute numerical derivatives by finite differencing. An adaptive algorithm is used to find the best choice of finite difference and to estimate the error in the derivative.\"
    derivCentral, derivForward, derivBackward,
    -- ** Multidimensional minimization
    -- | Minimization of a multidimensional function using some of the algorithms described in <http://www.gnu.org/software/gsl/manual/gsl-ref_35.html#SEC474>.
    minimizeConjugateGradient, minimizeNMSimplex, 
    -- ** Special functions
    erf, erf_Z,
    -- * Matrix manipulation
    size, rows, cols, diag, trans, conj, subVector, subMatrix,  
    flatten, reshape, fromBlocks, format,
    
    module Complex
) where

import GSL.Core
import GSL.Wrappers
import GSL.Derived
import GSL.Interface
import GSL.Drawing
import Complex



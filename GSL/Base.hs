-----------------------------------------------------------------------------
{- |
Module      :  GSL.Base
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses -fffi and -fglasgow-exts

High level functional interface to selected algorithms in the GSL and basic infrastructure for manipulation of vectors and matrices.

-}
-----------------------------------------------------------------------------

module GSL.Base (
    -- * Linear Algebra
    -- ** Vectors and Matrices
    -- | The linear algebra algorithms in this library work with four concrete data types: 
    Vector, Matrix, ComplexVector, ComplexMatrix,
    {- | They are statically different and inmutable, with automatic garbage collection. They are implemented as Foreign pointers to C-style arrays of double with dimension information, corresponding to the GSL vectors and matrices. These types are internally similar to StorableArray's, but they are inmutable and can be used outside the IO monad.)
    -}
    fromList, toList,
    -- $VectorsAndMatrices
    
    -- ** Basic Linear Algebra
    (<>), (<\>), det, pnorm,
    -- ** Matrix factorizations
    eig, svd, qr, chol, lu,
    -- * Fourier Transform
    fft, ifft,
    -- * Numerical functions
    -- ** Multidimensional minimization
    -- | Minimization of a multidimensional function using some of the algorithms described in <http://www.gnu.org/software/gsl/manual/gsl-ref_35.html#SEC474>.
    minimizeConjugateGradient, minimizeNMSimplex, 
    module Data.Complex
) where

import GSL.Core
import GSL.Wrappers
import GSL.Derived
import GSL.Interface
import GSL.Drawing
import Data.Complex

{- $VectorsAndMatrices Vectors and matrices admit the operators in the Num and Floating classes in the element by element sense:

@x = 'GSL.Interface.realVector' [1,2,3]
y = 'GSL.Interface.realVector' [10,20,30]
\ 
\> 1 + 2*x - y
-7. -15. -23.
\ 
\> sin x ^2 + cos x ^2
1. 1. 1.@

@a = 'GSL.Interface.realMatrix' [[1,2],[3,5]]
\ 
\> 7*a - 2 * 'GSL.Derived.ident' 2
 5. 14.
21. 33.@

They can be shown and read:

@> 2 * read \"1 2 3.5 4 5\"::'Vector'
2. 4. 7. 8. 10.@

@\m = read \"1 2 3; 4 5:+2 6 \\n 0 0 0:+1\"::'ComplexMatrix'
\> m
1.      2.   3.
4.  5.+2.i   6.
0.      0.  1.i@

Since the Num operations have signature t->t->t, to combine real and complex vectors you can use 'Util.complex':

@z = 'GSL.Interface.complexVector' [1,3-'i',4+2*'i']
\> z + 'complex' x
2.  5.-1.i  7.+2.i@

@\> 'GSL.Interface.complexMatrix' [[1,2-'i'],[1+'i',5]] \/ 'GSL.Interface.complex' (3*a)
       0.333  0.333-0.167i
0.111+0.111i         0.333@

The matrix product is denoted by the operator (\<\>) explained in the next section:

@\> a * 'ident' 2
1. 0.
0. 5.
\ 
\> a \<\> 'ident' 2
1. 2.
3. 5.@

-}

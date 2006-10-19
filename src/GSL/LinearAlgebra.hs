{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSL.LinearAlgebra
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Some linear algebra algorithms, implemented by means of the GSL or Lapack.

(In construction...)

-}
-----------------------------------------------------------------------------
module GSL.LinearAlgebra (
    -- * Data types
    module Complex, module G.Matrix,
    -- * Basic linear algebra
    mXm, mXv, vXm, dot, outer, scale, add, pnorm,
    -- * Matrix factorizations
    eigS,
    eigH,
    svd,
    qr,
    chol,
    -- * Utilities
    pinv, pinvTol, i, eps
) where

import Complex
import GSL.Matrix
import GSL.Common
import GSL.Wrappers

------------------------------------------------------------------------------




{- | Pseudoinverse of a real matrix with the default tolerance used by GNU-Octave: the singular values less than max (rows, colums) * greatest singular value * 'eps' are ignored. See 'pinvTol'.

@\> let m = 'realMatrix' [[ 1, 2]
                     ,[ 5, 8]
                     ,[10,-5]]
\> pinv m
9.353e-3 4.539e-2  7.637e-2
2.231e-2 8.993e-2 -4.719e-2
\ 
\> m \<\> pinv m \<\> m
 1.  2.
 5.  8.
10. -5.@

-}
pinv :: Matrix -> Matrix
pinv m = pinvTol 1 m

{- | Pseudoinverse of a real matrix with the desired tolerance, expressed as a
multiplicative factor of the default tolerance used by GNU-Octave (see 'pinv').

@\> let m = 'realMatrix' [[1,0,    0]
                     ,[0,1,    0]
                     ,[0,0,1e-10]]
\ 
\> 'pinv' m 
1. 0.           0.
0. 1.           0.
0. 0. 10000000000.
\ 
\> pinvTol 1E8 m
1. 0. 0.
0. 1. 0.
0. 0. 1.@

-}
pinvTol :: Double -> Matrix -> Matrix
pinvTol t m = v `mXm` diag s' `mXm` trans u where
    (u,s,v) = svd m
    sl@(g:_) = toList s
    s' = fromList . map rec $ sl
    rec x = if x < g*tol then 1 else 1/x
    tol = (fromIntegral (max (rows m) (cols m)) * g * t * eps)


{- | The imaginary unit

@> 'ident' 3 \<\> i
1.i   0.   0.
 0.  1.i   0.
 0.   0.  1.i@

-}
i :: Complex Double
i = 0:+1 

{- | Machine precision of a Double.     

>> eps
> 2.22044604925031e-16

(The value used by GNU-Octave)

-}
eps :: Double
eps =  2.22044604925031e-16


{- | Outer product of two vectors.

@\> 'realVector' [1,2,3] \`outer\` 'complexVector' [7,0,2*'i',1+'i']
 7.  0.  2.i  1.+1.i
14.  0.  4.i  2.+2.i
21.  0.  6.i  3.+3.i@

-}
outer :: Field a => GSLVector a -> GSLVector a -> GSLMatrix a
outer u v = reshape 1 u `mXm` reshape (size v) v

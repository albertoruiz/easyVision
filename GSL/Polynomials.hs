{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSL.Polynomials
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Polynomials.

<http://www.gnu.org/software/gsl/manual/html_node/General-Polynomial-Equations.html#General-Polynomial-Equations>

-}
-----------------------------------------------------------------------------
module GSL.Polynomials (
    polySolve
) where

import GSL.Core

{- | Solution of general polynomial equations, using /gsl_poly_complex_solve/. For example,
     the three solutions of x^3 + 8 = 0

@\> polySolve $ 'realVector' [8,0,0,1]
-2.  1.+1.732i  1.-1.732i@

The example in the GSL manual: To find the roots of x^5 -1 = 0:

@\> 'GSL.Interface.toList' $ polySolve ('realVector' [-1, 0, 0, 0, 0, 1]) 
[(-0.8090169943749475) :+ 0.5877852522924731,
(-0.8090169943749475) :+ (-0.5877852522924731),
0.30901699437494734 :+ 0.9510565162951536,
0.30901699437494734 :+ (-0.9510565162951536),
1.0 :+ 0.0]@

-}  
polySolve :: Vector -> ComplexVector
polySolve x@(V n _) = createV "polySolve" (n-1) $ v c_polySolve x
foreign import ccall "gslaux.h polySolve" c_polySolve:: TVCV

{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      : GSL.Fourier
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Fourier Transform.

<http://www.gnu.org/software/gsl/manual/html_node/Fast-Fourier-Transforms.html#Fast-Fourier-Transforms>

-}
-----------------------------------------------------------------------------
module GSL.Fourier (
    fft,
    ifft
) where

import GSL.Types
import GSL.Wrappers

genfft code x@(V n p) = createV [p] "fft" n $ v (c_fft code) x
foreign import ccall "gslaux.h fft" c_fft ::  Int -> TCVCV


{- | fast 1D Fourier transform of a vector using /gsl_fft_complex_forward/. It uses the same scaling conventions as GNU Octave.

@> fft ('complexVector' [1,2,3,4])
10.  -2.+2.i  -2.  -2.-2.i@

-}
fft :: Vector (Complex Double) -> Vector (Complex Double)
fft = genfft 0

{- | inverse 'fft' using /gsl_fft_complex_inverse/.

@> ifft ('complexVector' [0,2-2*'i',0,2+2*'i'])
1.  1.  -1.  -1.@

-}
ifft :: Vector (Complex Double) -> Vector (Complex Double)
ifft = genfft 1

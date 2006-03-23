{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSL.Utils
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses -fffi and -fglasgow-exts

Utility functions.

-}
-----------------------------------------------------------------------------

module GSL.Utils (
    -- * Easy creation of vectors and matrices
    realVector, realMatrix, complexVector, complexMatrix, 
    -- * Matrix IO
    fromFile, toFile, gslReadMatrix,
    -- * Special matrices
    ident, constant, hilb, 
    -- * Additional matrix manipulation
    extractRows, fromRows, toRows, fromCols, toCols, flipud, fliprl,
    vmap, mmap, vzip, mzip, 
    -- * Drawing 
    -- | A number of elementary drawing functions are provided. Some of them use the Haskell Graphics library. External tools like gnuplot and imageMagick are also temporarily used; we will eventually HOpengGL. Some function names (after Octave) are provisional not very informative...
    hplot, plot, parametricPlot, plotOpenGL, mplot, 
    meshOpenGL, splot, mesh, meshdom, 
    matrixToPGM, imshow,
    -- * Other
     i, norm, (//), disp, sumCols, outer, linspace
) where

import GSL.Core
import GSL.Wrappers
import GSL.Derived
import GSL.Interface
import GSL.Drawing
import Foreign
import Complex

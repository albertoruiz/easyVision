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
    fromStorableArrayV, fromStorableArrayM, toStorableArrayV, toStorableArrayM,
    -- * Matrix IO
    fromFile, toFile, gslReadMatrix,
    -- * Special matrices
    ident, constant, hilb, 
    -- * Additional matrix manipulation
    extractRows, fromRows, toRows, fromCols, toCols, flipud, fliprl,
    vmap, mmap, vzip, mzip, 
    -- * Drawing 
    -- | A number of elementary drawing functions using HGL and HOpenGL. External tools (gnuplot and imageMagick) can also be used. The function names (after Octave) are provisional...
    hplot, plot, parametricPlot, plotOpenGL, mplot, 
    meshOpenGL, splot, mesh, meshdom, 
    matrixToPGM, imshow,
    -- * Other
     i, complex, norm, pinv, pinvTol, eps, disp, (//), sumCols, outer, linspace
) where

import GSL.Core
import GSL.Wrappers
import GSL.Derived
import GSL.Interface
import GSL.Drawing
import Foreign
import Complex

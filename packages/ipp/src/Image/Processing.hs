module Image.Processing(
    -- * Basic functions
    Image(), Pix(), size, roi, setROI, modifyROI,
    G.constant, set, copy,
    -- * spatial transformations
    resize, G.resizeFull, G.warp, warpon, uradial,
    -- * Logical functions
    andI, orI, notI, xorI, dilate3x3, erode3x3,
    -- * Arithmetic functions
    (.*),(.+),
    (|+|),(|-|),absDiff,(|*|),(|/|),
    addC8u, add8u, absDiff8u, sub8u, sub8uRel,
    -- * Image filters and matching
    crossCorr, sqrDist,
    -- * Color transformations
    twistColors
) where

import Image.Core
import ImagProc.Ipp.Generic(Pix)
import qualified ImagProc.Ipp.Generic as G
import ImagProc.Ipp.Pure
import ImagProc.Ipp.AdHoc(twistColors)


set  x l = G.set  x l
copy x l = G.copy x l
resize s i = G.resize s i
warpon i l = G.warpon i l
uradial f g i = G.uradial f g i
crossCorr a b = G.crossCorr a b
sqrDist a b = G.sqrDist a b


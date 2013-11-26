module Image.Processing(
    -- * Image
    Image(), size, roi, setROI, modifyROI,
    -- * Generic functions
    Pix(),
    G.constant, set, copy, resize, G.resizeFull, G.warp, warpon, uradial,
    -- * Logical functions
    andI, orI, notI, xorI, dilate3x3, erode3x3,
    -- * Arithmetic functions
    (.*),(.+),
    (|+|),(|-|),absDiff,(|*|),(|/|),
    addC8u, add8u, absDiff8u, sub8u, sub8uRel
) where

import Image.Core
import ImagProc.Ipp.Generic(Pix)
import qualified ImagProc.Ipp.Generic as G
import ImagProc.Ipp.Pure


set  x l = G.set  x l
copy x l = G.copy x l
resize s i = G.resize s i
warpon i l = G.warpon i l
uradial f g i = G.uradial f g i

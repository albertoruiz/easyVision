module Image.Processing(
    -- * the image type
    Image(),
    module Image,
    -- * basic functions
    size, roi, setROI, modifyROI,
    G.constant, set, zeroP, copy, G.blockImage,
    -- * spatial transformations
    resize, G.resizeFull, G.warp, warpon, uradial,
    -- * logical functions
    andI, orI, notI, xorI, dilate3x3, erode3x3,
    -- * arithmetic functions
    (.*),(.+),
    (|+|),(|-|),absDiff,(|*|),(|/|), abs32f, sqrt32f,
    addC8u, add8u, sub8u, sub8uRel,
    sumPixels,
    -- * threshold and comparison
    (G..<.),(G..>.),
    thresholdVal32f, thresholdVal8u,
    compareC8u, compare8u, IppCmp(..),
    minmax,maxIdx,
    maxEvery, minEvery,
    maxEvery8u, minEvery8u,
    otsuThreshold, G.otsuBinarize,
    -- * histograms
    histogram, histogramN,
    -- * filters
    convolution,
    filterMax, filterMin,
    filterBox, filterMedian,
    sobelVert, sobelHoriz,
    gauss, laplace, median, highPass8u, Mask(..),
    gaussS, gaussS', hessian, Grads(..), gradients,
    -- * matching
    crossCorr, G.crossCorrLoc, sqrDist,
    -- * contours
    contours, localContours,
    -- * color transformations
    rgbToGray, rgbToHSV, hsvToRGB, yCbCrToRGB, rgbToYCbCr,
    twistColors, G.Channels(..), G.channelsFromRGB, G.channelsFromYUYV, G.grayscale, G.grayf,
    -- * pixel transformations
    toFloat, toGray, scale32f8u, scale8u32f,
    -- * misc
    ippSetNumThreads,
    floodFill8u, floodFill8uGrad,
    -- * types
    Pix(), NPix(), Size(..), ROI(..),
    Point(..), Polyline(..), Segment(..)
) where

import Image
import Util.Geometry
import ImagProc.Ipp.Generic(Pix,NPix)
import qualified ImagProc.Ipp.Generic as G
import ImagProc.Ipp.Tools
import ImagProc.Ipp.Pure
import ImagProc.Ipp.AdHoc
import ImagProc.Ipp.Contour
import ImagProc.Ipp.Wrappers(ippSetNumThreads)
import ImagProc.Ipp.Structs

set  x l = G.set  x l
copy x l = G.copy x l
resize s i = G.resize s i
warpon i l = G.warpon i l
uradial f g i = G.uradial f g i
crossCorr a b = G.crossCorr a b
sqrDist a b = G.sqrDist a b
absDiff a b = G.absDiff a b
sumPixels x = G.sumPixels x
convolution m x = G.convolution m x
filterMax k x = G.filterMax k x
filterMin k x = G.filterMin k x
filterBox i j x = G.filterBox i j x
gauss m x = G.gauss m x
zeroP :: Pix p => p
zeroP = G.zeroP

maxIdx x = G.maxIdx x


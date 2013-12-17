module Image.Processing(
    -- * the image type
    Image(),
    module Image,
    -- * basic functions
    size, roi, setROI, modifyROI,
    G.constantImage, set, zeroP, copy, copyMask, G.blockImage,
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
    (G..<.),(G..>.), compareImages, IppCmp(..),
    thresholdVal,
    minmax,maxIdx,
    maxEvery, minEvery,
    otsuThreshold, G.otsuBinarize,
    getPoints,
    -- * histograms
    histogram, histogramN,
    -- * filters
    Mask(..), gauss, median, laplace, highPass8u,
    sobelVert, sobelHoriz,
    filterMax, filterMin,
    filterBox, filterMedian,
    localMax,
    convolution,
    gaussS, gaussS', Grads(..), gradients, hessian, canny,
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
    compareC8u,
    -- * domain transformations
    ImageBasis(..),
    imageBasis,
    momentsImage,
    domainTrans32f,
    -- * types
    Pix(), NPix(), Size(..), ROI(..),
    Point(..), Polyline(..), Segment(..)
) where

import Image
import Util.Geometry
import Image.Processing.Generic(Pix,NPix)
import qualified Image.Processing.Generic as G
import Image.Processing.Tools
import Image.Processing.IPP
import Image.Processing.Contour
import Image.Processing.Moments
import Image.Processing.Custom(domainTrans32f)

--import ImagProc.Ipp.Wrappers(ippSetNumThreads)
--import ImagProc.Ipp.Structs

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
compareImages c x y = G.compareImages c x y
getPoints n x = G.getPoints n x


maxIdx x = G.maxIdx x

copyMask :: Pix p
         => Image p   -- ^ base image
         -> Image p   -- ^ source image
         -> Image I8u -- ^ mask
         -> Image p
copyMask = G.copyMask
minEvery a b = G.minEvery a b
maxEvery a b = G.maxEvery a b

thresholdVal :: NPix p
    => p          -- ^ threshold
    -> p          -- ^ value
    -> IppCmp     -- ^ comparison function
    -> Image p    -- ^ source image
    -> Image p    -- ^ result
thresholdVal t v a c = G.thresholdVal t v a c



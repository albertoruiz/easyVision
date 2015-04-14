module Image.Processing(
    -- * the image type
    Image(),
    module Image,
    -- * basic functions
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
    mirror8u,
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
import Numeric.LinearAlgebra.HMatrix

--import ImagProc.Ipp.Wrappers(ippSetNumThreads)
--import ImagProc.Ipp.Structs

set :: Pix p => Image p -> [(ROI, p)] -> Image p
set  x l = G.set  x l

copy :: Pix p => Image p -> [(Image p, Pixel)] -> Image p
copy x l = G.copy x l

resize :: Pix p => Size -> Image p -> Image p
resize s i = G.resize s i

warpon
  :: Pix p =>
     Image p -> [(Matrix Double, Image p)] -> Image p
warpon i l = G.warpon i l

uradial :: Pix p => Float -> Float -> Image p -> Image p
uradial f g i = G.uradial f g i

crossCorr :: Pix p => Image p -> Image p -> Image Float
crossCorr a b = G.crossCorr a b

sqrDist :: Pix p => Image p -> Image p -> Image Float
sqrDist a b = G.sqrDist a b

absDiff :: Pix p => Image p -> Image p -> Image p
absDiff a b = G.absDiff a b

sumPixels :: Pix p => Image p -> Double
sumPixels x = G.sumPixels x

convolution :: NPix p => [[Float]] -> Image p -> Image p
convolution m x = G.convolution m x

filterMax :: NPix p => Int -> Image p -> Image p
filterMax k x = G.filterMax k x

filterMin :: NPix p => Int -> Image p -> Image p
filterMin k x = G.filterMin k x

filterBox :: NPix p => Int -> Int -> Image p -> Image p
filterBox i j x = G.filterBox i j x

gauss :: NPix p => Mask -> Image p -> Image p
gauss m x = G.gauss m x

zeroP :: Pix p => p
zeroP = G.zeroP

compareImages :: NPix p => IppCmp -> Image p -> Image p -> Image I8u
compareImages c x y = G.compareImages c x y

getPoints :: NPix p => Int -> Image p -> [Point]
getPoints n x = G.getPoints n x


maxIdx :: NPix p => Image p -> (p, Pixel)
maxIdx x = G.maxIdx x

copyMask :: Pix p
         => Image p   -- ^ base image
         -> Image p   -- ^ source image
         -> Image I8u -- ^ mask
         -> Image p
copyMask = G.copyMask

minEvery :: NPix p => Image p -> Image p -> Image p
minEvery a b = G.minEvery a b

maxEvery :: NPix p => Image p -> Image p -> Image p
maxEvery a b = G.maxEvery a b

thresholdVal :: NPix p
    => p          -- ^ threshold
    -> p          -- ^ value
    -> IppCmp     -- ^ comparison function
    -> Image p    -- ^ source image
    -> Image p    -- ^ result
thresholdVal t v a c = G.thresholdVal t v a c



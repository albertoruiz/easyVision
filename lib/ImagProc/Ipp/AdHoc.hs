-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Ipp.Manual
Copyright   :  (c) Alberto Ruiz 2006/8
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

IPP functions which need an ad-hoc wrapper

-}
-----------------------------------------------------------------------------

module ImagProc.Ipp.AdHoc
where

import ImagProc.Ipp.Core
import ImagProc.Ipp.Adapt
import ImagProc.Ipp.Auto
import ImagProc.Ipp.Wrappers
import Foreign hiding (shift)
import Foreign.C.Types(CUChar,CInt)


-- | Writes into a existing image a desired value in a specified roi.
set32f :: Float      -- ^ desired value
       -> ROI        -- ^ roi
       -> ImageFloat -- ^ destination image
       -> IO ()
set32f v roi (F im) = ippiSet_32f_C1R v // dst im roi // checkIPP "set32f" [im]

-- | Writes into a existing image a desired value in a specified roi.
set8u :: CUChar      -- ^ desired value
       -> ROI        -- ^ roi
       -> ImageGray  -- ^ destination image
       -> IO ()
set8u v roi (G im) = ippiSet_8u_C1R v // dst im roi // checkIPP "set8u" [im]


-- | Writes into a existing image a desired value in a specified roi.
set8u3 :: CUChar -> CUChar -> CUChar -- ^ desired RGB value
       -> ROI        -- ^ roi
       -> ImageRGB   -- ^ destination image
       -> IO ()
set8u3 r g b roi (C im) = do
    v <- mallocArray 3
    pokeArray v [r,g,b]
    ippiSet_8u_C3R v // dst im roi // checkIPP "set8u3" [im]
    free v

----------------------------------------------------------------------------

-- | Copies the roi of the input image into the roi of the destination image.
copyROI32f :: ImageFloat -- ^ input image
            -> ROI
            -> ImageFloat -- ^ destination image
            -> ROI
            -> IO ()
copyROI32f (F im) r1 (F r) r2 = ippiCopy_32f_C1R // src im r1 // dst r r2 // checkIPP "copyROI32f'" [im]

-- | Copies the roi of the input image into the roi of the destination image.
copyROI8u :: ImageGray -> ROI -> ImageGray -> ROI -> IO ()
copyROI8u (G im) r1 (G r) r2 = ippiCopy_8u_C1R // src im r1 // dst r r2 // checkIPP "copyROI8u'" [im]

-- | Copies the roi of the input image into the roi of the destination image.
copyROI8u3 :: ImageRGB -> ROI -> ImageRGB -> ROI -> IO ()
copyROI8u3 (C im) r1 (C r) r2 = ippiCopy_8u_C3R // src im r1 // dst r r2 // checkIPP "copyROI8u3'" [im]

--------------------------------------------------------------------
inter_NN         =  1 :: Int
inter_LINEAR     =  2 :: Int
inter_CUBIC      =  4 :: Int
inter_SUPER      =  8 :: Int
inter_LANCZOS    = 16 :: Int
--inter_SMOOTH_EDGE = (1 << 31) :: Int

genResize f s dst droi im sroi interp = do
               f (ptr im) (step im) (height $ isize im) (width $ isize im)
                 (r1 sroi) (r2 sroi) (c1 sroi) (c2 sroi)
                 (ptr dst) (step dst)
                 (r1 droi) (r2 droi) (c1 droi) (c2 droi)
                 interp // checkIPP s [im]

-- | Resizes the roi of a given image.
resize32f :: Size -> ImageFloat -> ImageFloat
resize32f s (F im) = unsafePerformIO $ do
    r <- img I32f s
    genResize c_resize32f "genResize32f" r (fullroi r) im (vroi im) inter_LINEAR
    return (F r)

-- | Resizes the roi of a given image.
resize8u :: Size -> ImageGray -> ImageGray
resize8u s (G im) = unsafePerformIO $ do
    r <- img Gray s
    genResize c_resize8u "genResize8u" r (fullroi r) im (vroi im) inter_LINEAR
    return (G r)

-- | Resizes the roi of a given image.
resize8u3 :: Size -> ImageRGB -> ImageRGB
resize8u3 s (C im) = unsafePerformIO $ do
    r <- img RGB s
    genResize c_resize8u3 "genResize8u3" r (fullroi r) im (vroi im) inter_LINEAR
    return (C r)

-- | Resizes the full image and its roi
resize32f' :: Size -> ImageFloat -> ImageFloat
resize32f' s (F im) = unsafePerformIO $ do
    r <- img I32f s
    genResize c_resize32f "genResize32f" r (fullroi r) im (fullroi im) inter_LINEAR
    let Size h' w' = s
        Size h w = isize im
        fh = fromIntegral h' / fromIntegral h
        fw = fromIntegral w' / fromIntegral w
        ROI r1 r2 c1 c2 = vroi im
        f n = fromIntegral n
        newroi = ROI (ceiling (fh*f r1)) (floor (fh*f r2)) (ceiling(fw*f c1)) (floor(fw*f c2))
    return (F r {vroi = newroi})


----------------------------------------------------------------------------------------

warpOn' h r im f met s = do
    coefs <- newArray (concat h)
    let Size h w = isize im
    f (ptr im) (step im) h w
                           (r1 (vroi im)) (r2 (vroi im)) (c1 (vroi im)) (c2 (vroi im))
                           (ptr r) (step r)
                           (r1 (vroi r)) (r2 (vroi r)) (c1 (vroi r)) (c2 (vroi r))
                           coefs met //warningIPP s [im]
    free coefs

warpOn8u  h (G r) (G im) = warpOn' h r im warpPerspectiveGray inter_LINEAR "warpOn8u"
warpOn32f h (F r) (F im) = warpOn' h r im warpPerspective32f inter_LINEAR "warpOn32f"
warpOn8u3 h (C r) (C im) = warpOn' h r im warpPerspectiveRGB inter_LINEAR "warpOn8u3"

-------------------------------------------------------------------

-- | extracts a given channel of a 8uC3 image into a 8uC1 image
getChannel :: Int -> ImageRGB -> ImageGray
getChannel c (C im) = unsafePerformIO $ do
    G r <- image (isize im)
    let roi = vroi im
        im' = im {ptr = ptr im `plusPtr` c}
    ippiCopy_8u_C3C1R // src im' roi // dst r roi // checkIPP "ippiCopy_8u_C3C1R" [im]
    return $ modifyROI (const roi) (G r)

-- | Creates an 8uC3 image from three 8uC1 images. (to do consistency)
putChannels :: (ImageGray,ImageGray,ImageGray) -> ImageRGB
putChannels (G r, G g, G b) = unsafePerformIO $ do
    C c <- image (isize r)
    let roi = vroi r
        r' = c {ptr = ptr c `plusPtr` 0}
        g' = c {ptr = ptr c `plusPtr` 1}
        b' = c {ptr = ptr c `plusPtr` 2}
    ippiCopy_8u_C1C3R // src r roi // dst r' roi // checkIPP "ippiCopy_8u_C1C3R-1" [c]
    ippiCopy_8u_C1C3R // src g roi // dst g' roi // checkIPP "ippiCopy_8u_C1C3R-2" [c]
    ippiCopy_8u_C1C3R // src b roi // dst b' roi // checkIPP "ippiCopy_8u_C1C3R-3" [c]
    return $ modifyROI (const roi) (C c)

-----------------------------------------------------------------------

-- | Creates a 8u gray image from a YUV420 image (typically generated by a MPlayer camera)
yuvToGray :: ImageYUV   -- ^ input image
          -> ImageGray  -- ^ result
yuvToGray (Y im) = G im {layers = 1, itype = Gray, step = width (isize im)}

-- | the inverse of yuvToGray (the U and V channels are filled with 128).
grayToYUV :: ImageGray    -- ^ input image
          -> ImageYUV     -- ^ result
grayToYUV (G im) = unsafePerformIO $ do
    res <- img YUV (isize im)
    let Size h w = isize im
        tot = h*w `div` 2
    z@(G zero) <- image (Size 1 tot) -- hack to clear memory
    set8u 128 (theROI z) z
    copyBytes (ptr res `plusPtr` (h*w)) (ptr zero) tot
    copyBytes (ptr res) (ptr im) (h*step im)
    touchForeignPtr (fptr im)
    return (Y res)

-- | Creates a 8uC3R RGB image from a YUV420 image (typically generated by a MPlayer camera). TODO : only ROI
yuvToRGB :: ImageYUV  -- ^ input image
         -> ImageRGB  -- ^ result
yuvToRGB (Y im) = unsafePerformIO $ do
    r' <- img RGB (isize im)
    let r = r' {vroi = vroi im}
    psrc  <- mallocArray 3
    let Size h w = isize im
    let ps = castPtr (ptr im) :: Ptr CUChar
    pokeArray psrc [ps, ps `advancePtr` (h*w), ps `advancePtr` (h*w + h*w `div` 4)]
    pstep <- mallocArray 3
    pokeArray pstep (map fi [w, w`div`2, w`div`2])
    ippiYUV420ToRGB_8u_P3C3R (castPtr psrc) pstep (castPtr $ ptr r) (step r) (roiSZ (fullroi r)) // checkIPP "yuvToRGB" [im]
    free psrc
    free pstep
    return (C r)


-- | Creates  a YUV420 image (typically generated by a MPlayer camera) from a 8uC3R RGB image. TODO : only ROI
rgbToYUV :: ImageRGB  -- ^ input image
         -> ImageYUV  -- ^ result
rgbToYUV (C im) = unsafePerformIO $ do
    r' <- img YUV (isize im)
    let r = r' {vroi = vroi im}
    pdst  <- mallocArray 3
    let Size h w = isize im
    let ps = castPtr (ptr r) :: Ptr CUChar
    pokeArray pdst [ps, ps `advancePtr` (h*w), ps `advancePtr` (h*w + h*w `div` 4)]
    pstep <- mallocArray 3
    pokeArray pstep (map fi [w, w`div`2, w`div`2])
    ippiRGBToYUV420_8u_C3P3R (castPtr $ ptr im) (step im) (castPtr pdst) pstep (roiSZ (fullroi r)) // checkIPP "rgbToYUV" [im]
    free pdst
    free pstep
    return (Y r)

------------------------------------------------------------------

-- | Fills (as a side effect) a connected component in the image, starting at the seed pixel. It returns
-- the enclosing ROI, area and value. This is the 8con version.
floodFill8u :: ImageGray -> Pixel -> CUChar -> IO (ROI, Int, CUChar)
floodFill8u (G im) (Pixel r c) val = do
    let roi@(ROI r1 r2 c1 c2) = vroi im
    pregion <- malloc
    pbufsize <- malloc
    (ippiFloodFillGetSize (roiSZ roi)) pbufsize // checkIPP "ippiFloodFillGetSize" []
    bufsize <- peek pbufsize
    buf <- mallocBytes (fromIntegral bufsize)
    free pbufsize
    (ippiFloodFill_8Con_8u_C1IR // dst im (vroi im)) (IppiPoint (fi $ c-c1) (fi $ r-r1)) val pregion buf // checkIPP "ippiFloodFill_8Con_8u_C1IR" [im]
    free buf
    IppiConnectedComp area value0 _ _ (IppiRect x y w h) <- peek pregion
    free pregion
    return (ROI (r1+ ti y) (r1+ti y+ti h-1) (c1+ti x) (c1+ti x+ti w-1), round area, round value0)

-- | Fills (as a side effect) a connected component in the image, starting at the seed pixel.
-- This version admits a lower and higher difference in the pixel values.
-- It returns the enclosing ROI, area and value. This is the 8con version.
floodFill8uGrad :: ImageGray -> Pixel -> CUChar -> CUChar -> CUChar-> IO (ROI, Int, CUChar)
floodFill8uGrad (G im) (Pixel r c) dmin dmax val = do
    let roi@(ROI r1 r2 c1 c2) = vroi im
    pregion <- malloc
    pbufsize <- malloc
    ippiFloodFillGetSize (roiSZ roi) pbufsize // checkIPP "ippiFloodFillGetSize" []
    bufsize <- peek pbufsize
    buf <- mallocBytes (fromIntegral bufsize)
    free pbufsize
    (ippiFloodFill_Grad8Con_8u_C1IR // dst im (vroi im)) (IppiPoint (fi $ c-c1) (fi $ r-r1)) val dmin dmax pregion buf // checkIPP "ippiFloodFill_Grad8Con_8u_C1IR" [im]
    free buf
    IppiConnectedComp area value0 _ _ (IppiRect x y w h) <- peek pregion
    free pregion
    return (ROI (r1+ ti y) (r1+ti y+ti h-1) (c1+ti x) (c1+ti x+ti w-1), round area, round value0)

----------------------------------------------------------------

-- | Returns the minimum and maximum value in an image32f
minmax :: ImageFloat -> (Float,Float)
minmax (F im) = unsafePerformIO $ do
    mn <- malloc 
    mx <- malloc
    (ippiMinMax_32f_C1R // dst im (vroi im)) mn mx // checkIPP "minmax" [im]
    a <- peek mn
    b <- peek mx
    free mn
    free mx
    return (a,b)


-- | Returns the maximum value and its position in the roi of an image32f. The position is relative to the ROI.
maxIndx :: ImageFloat -> (Float,Pixel)
maxIndx (F im) = unsafePerformIO $ do
    mx <- malloc
    px <- malloc
    py <- malloc
    (ippiMaxIndx_32f_C1R // dst im (vroi im)) mx px py // checkIPP "maxIndx" [im]
    v <- peek mx
    x <- peek px
    y <- peek py
    free mx
    free px
    free py
    return (v,Pixel (fromIntegral y) (fromIntegral x))

-- | Returns the maximum value and its position in the roi of an image32f. The position is relative to the image.
maxIndx32f :: ImageFloat -> (Float,Pixel)
maxIndx32f (F im) = unsafePerformIO $ do
    let roi@(ROI r1 r2 c1 c2) = vroi im
    mx <- malloc
    px <- malloc
    py <- malloc
    (ippiMaxIndx_32f_C1R // dst im (vroi im)) mx px py // checkIPP "maxIndx" [im]
    v <- peek mx
    x <- peek px
    y <- peek py
    free mx
    free px
    free py
    return (v,Pixel (r1+fromIntegral y) (c1+fromIntegral x))

-- | Returns the maximum value and its position in the roi of an image8u. The position is relative to the image.
maxIndx8u :: ImageGray -> (CUChar,Pixel)
maxIndx8u (G im) = unsafePerformIO $ do
    let roi@(ROI r1 r2 c1 c2) = vroi im
    mx <- malloc
    px <- malloc
    py <- malloc
    (ippiMaxIndx_8u_C1R // dst im roi) mx px py // checkIPP "maxIndx8u" [im]
    v <- peek mx
    x <- peek px
    y <- peek py
    free mx
    free px
    free py
    return (v,Pixel (r1 + fromIntegral y) (c1 + fromIntegral x))

----------------------------------------------------------------------

-- | The result is the pixelswise comparation of the two source images.
compare32f :: IppCmp -> ImageFloat -> ImageFloat -> ImageGray
compare32f cmp (F im1) (F im2) = unsafePerformIO $ do
    r <- img Gray (isize im1)
    let roi = intersection (vroi im1) (vroi im2)
    (ippiCompare_32f_C1R // src im1 roi // src im2 roi // dst r roi) (codeCmp cmp) // checkIPP "compare32f" [im1,im2]
    return (G r {vroi = roi})

-- | Creates a copy of the source image only on corresponding pixels in which mask=255
copyMask32f :: ImageFloat    -- ^ source image
            -> ImageGray     -- ^ mask image
            -> ImageFloat    -- ^ result
copyMask32f (F im) (G mask) = unsafePerformIO $ do
    r <- imgAs im
    let roi = intersection (vroi im) (vroi mask)
    set32f 0.0 (fullroi r) (F r)
    ippiCopy_32f_C1MR // src im roi // dst r roi // src mask roi // checkIPP "copyMask32f" [im,mask]
    return $ F r {vroi = roi}

-- | Sum of all pixels in the roi a 8u image
sum8u :: ImageGray -> Double
sum8u (G im) = unsafePerformIO $ do
    pf <- malloc
    (ippiSum_8u_C1R // dst im (vroi im)) pf // checkIPP "sum8u" [im]
    r <- peek pf
    free pf
    return r

sum32f :: ImageFloat -> Double
sum32f (F im) = unsafePerformIO $ do
    pf <- malloc
    (ippiSum_32f_C1R // dst im (vroi im)) pf (codeAlgHint AlgHintNone) // checkIPP "sum32f" [im]
    r <- peek pf
    free pf
    return r

---------------------------------------------------------

-- | Discrete cosine transform of a 32f image.
dct :: ImageFloat -> ImageFloat
dct = genDCT auxDCTFwd_32f_C1R "dct"

-- | Inverse discrete cosine transform of a 32f image.
idct :: ImageFloat -> ImageFloat
idct = genDCT auxDCTInv_32f_C1R "idct"

genDCT auxfun name (F im) = unsafePerformIO $ do
    F r' <- image (isize im)
    let r = r' {vroi = vroi im}
    auxfun (castPtr (ptr im)) (step im)
           (r1 (vroi im)) (r2 (vroi im)) (c1 (vroi im)) (c2 (vroi im))
           (castPtr (ptr r)) (step r)
           (r1 (vroi r)) (r2 (vroi r)) (c1 (vroi r)) (c2 (vroi r))
           // checkIPP name [im]
    return (F r)

------------------------------------------------------------------------

-- | Creates a function to compute the FFT of a 32f image. The resulting function produces 32f images in complex packed format. The dimensions of the ROI must be powers of two.
genFFT :: Int -- ^ ordx
       -> Int -- ^ ordy
       -> FFTNormalization
       -> AlgHint
       -> IO (ImageFloat -> ImageFloat) -- ^ resulting FFT function
genFFT ordx ordy flag alg = do
    ptrSt <- malloc
    ippiFFTInitAlloc_R_32f (castPtr ptrSt) ordx ordy (codeFFTFlag flag) (codeAlgHint alg) // checkIPP "FFTInitAlloc" []
    st <- peek ptrSt
    pn <- malloc
    ippiFFTGetBufSize_R_32f st pn // checkIPP "FFTGetBufSize" []
    n <- peek pn
    buf <- mallocBytes (fromIntegral n)
    let fft (F im) = unsafePerformIO $ do
        F r' <- image (isize im)
        let r = r' {vroi = vroi im}
        (ippiFFTFwd_RToPack_32f_C1R // src im (vroi im) // src r (vroi r)) st buf // checkIPP "FFTFwd_RToPack_32f_C1R" [im]
        return (F r)
    return fft
-- free?


-- | Relocates the low frequencies of 'magnitudePack' in the center of the ROI.
powerSpectrum :: ImageFloat -> ImageFloat
powerSpectrum (F im) = unsafePerformIO $ do
    F r <- image (isize im)
    let ROI r1 r2 c1 c2 = vroi im
    set32f 0 (vroi im) (F r)
    let cm = ((c2-c1+1) `div` 2)
    let rm = ((r2-r1+1) `div` 2)
    let sroi = ROI r1 (r1+rm-1) c1 (c1+cm-1)
    let droi = shift (rm,cm) sroi
    ippiCopy_32f_C1R // src im sroi // dst r droi // checkIPP "powerSpectrum-1" [im]
    let droi = ROI r1 (r1+rm-1) c1 (c1+cm-1)
    let sroi = shift (rm,cm) droi
    ippiCopy_32f_C1R // src im sroi // dst r droi // checkIPP "powerSpectrum-2" [im]
    let sroi = ROI r1 (r1+rm-1) (c1+cm) c2
    let droi = shift (rm,-cm) sroi
    ippiCopy_32f_C1R // src im sroi // dst r droi // checkIPP "powerSpectrum-3" [im]
    let droi = ROI r1 (r1+rm-1) (c1+cm) c2
    let sroi = shift (rm,-cm) droi
    ippiCopy_32f_C1R // src im sroi // dst r droi // checkIPP "powerSpectrum-4" [im]
    return (F r {vroi = vroi im})

-------------------------------------------------

-- | Distance transform: Given an 8u image with feature pixels = 0, computes a 32f image with the distance from each pixel to the nearest feature pixel. The argument metrics is a list of float with two (for a 3x3 mask) or three elements (for a 5x5 mask), which specify respectively the distances between pixels which share an edge, a corner and pixels at distance of chess knight move. For example, for L2 metrics we use [1,1.4] (3x3 mask) or [1,1.4,2.2] (5x5 mask). If metrics is not valid (e.g. []), then [1,1.4] is used.
distanceTransform :: [Float]       -- ^ metrics
                  -> ImageGray     -- ^ source image 
                  -> ImageFloat -- ^ result

distanceTransform (m@[_,_]) = genDistanceTransform ippiDistanceTransform_3x3_8u32f_C1R m
distanceTransform (m@[_,_,_]) = genDistanceTransform ippiDistanceTransform_5x5_8u32f_C1R m
distanceTransform _ = distanceTransform [1,1.4]

genDistanceTransform f metrics (G im) = unsafePerformIO $ do
    pmetrics <- newArray metrics
    r' <- img I32f (isize im)
    let r = r' {vroi = vroi im}
    (f // src im (vroi im) // dst r (vroi r)) pmetrics // checkIPP "ippiDistanceTransform_?_8u32f_C1R" [im]
    free pmetrics
    return (F r)

-------------------------------------------------------------------------

-- | Canny's edge detector.
canny :: (ImageFloat,ImageFloat) -- ^ image gradient (dx,dy)
      -> (Float,Float)           -- ^ low and high threshold
      -> ImageGray               -- ^ resulting image
canny (F dx, F dy) (l,h) = unsafePerformIO $ do
    r <- img Gray (isize dx)
    ps <- malloc
    let roi = intersection (vroi dx) (vroi dy)
    (ippiCannyGetSize (roiSZ roi)) ps // checkIPP "ippiCannyGetSize" []
    s <- peek ps
    buffer <- mallocBytes (fromIntegral s)
    (ippiCanny_32f8u_C1R // src dx roi // src dy roi // dst r roi) l h buffer
                         // checkIPP "ippiCanny_32f8u_C1R" [dx,dy]
    free buffer
    free ps
    return (G r {vroi = roi})

--------------------------------------------------------------------------------

-- | Histogram of a 8u image. For instance, @histogram [0,64 .. 256] g@ computes an histogram with four bins equally spaced in the image range.
histogram :: [Int] -- ^ bin bounds
          -> ImageGray -- ^ source image
          -> [Int]     -- result
histogram bins (G im) = unsafePerformIO $ do
    let n = length bins
    pbins <- newArray (map fromIntegral bins)
    pr <- mallocArray n
    (ippiHistogramRange_8u_C1R // dst im (vroi im)) pr pbins n // checkIPP "histogram" [im]
    r <- peekArray (n-1) pr
    free pbins
    free pr
    return (map fromIntegral r)

-- normalized histogram
histogramN bins im = map ((*sc).fromIntegral) h where
    h = histogram bins im
    ROI r1 r2 c1 c2 = theROI im
    sc = (1.0::Double) / fromIntegral ((r2-r1+1)*(c2-c1+1))

-----------------------------------------------------------------------------------

convolution32f :: [[Float]] -> ImageFloat -> ImageFloat
convolution32f mask img = unsafePerformIO $ do
    pKernel <- newArray (concat mask)
    let r = fi $ length mask
        rm = r `div` 2
        c = fi $ length (head mask)
        cm = c `div` 2
    r <- ioFilter_32f_C1R pKernel (IppiSize r c) (IppiPoint cm rm) (shrink (ti rm,ti cm)) img
    free pKernel
    return r

convolution8u :: [[Int]] -> Int -> ImageGray -> ImageGray
convolution8u mask divisor img = unsafePerformIO $ do
    pKernel <- newArray (map fromIntegral $ concat mask)
    let r = fi $ length mask
        rm = r `div` 2
        c = fi $ length (head mask)
        cm = c `div` 2
    r <- ioFilter_8u_C1R pKernel (IppiSize r c) (IppiPoint cm rm) divisor (shrink (ti rm,ti cm)) img
    free pKernel
    return r

convolutionColumn32f :: [Float] -> ImageFloat -> ImageFloat
convolutionColumn32f mask img = unsafePerformIO $ do
    pKernel <- newArray mask
    let r = length mask
        rm = r `div` 2
    r <- ioFilterColumn_32f_C1R pKernel r rm (shrink (rm,0)) img
    free pKernel
    return r

convolutionColumn8u :: [Int] -> Int -> ImageGray -> ImageGray
convolutionColumn8u mask divisor img = unsafePerformIO $ do
    pKernel <- newArray (map fromIntegral mask)
    let r = length mask
        rm = r `div` 2
    r <- ioFilterColumn_8u_C1R pKernel r rm divisor (shrink (rm,0)) img
    free pKernel
    return r

convolutionRow32f :: [Float] -> ImageFloat -> ImageFloat
convolutionRow32f mask img = unsafePerformIO $ do
    pKernel <- newArray mask
    let r = length mask
        rm = r `div` 2
    r <- ioFilterRow_32f_C1R pKernel r rm (shrink (0,rm)) img
    free pKernel
    return r

convolutionRow8u :: [Int] -> Int -> ImageGray -> ImageGray
convolutionRow8u mask divisor img = unsafePerformIO $ do
    pKernel <- newArray (map fromIntegral mask)
    let r = length mask
        rm = r `div` 2
    r <- ioFilterRow_8u_C1R pKernel r rm divisor (shrink (0,rm)) img
    free pKernel
    return r

-----------------------------------------------------------------------------------

sampleLine8u :: ImageGray -> Pixel -> Pixel -> [CUChar]
sampleLine8u (G img) (Pixel pr1 pc1) (Pixel pr2 pc2) = unsafePerformIO $ do
    let roi = vroi img
        n = max (abs(pr2-pr1+1)) (abs(pc2-pc1+1))
    pline <- mallocArray n
    let pt1 = IppiPoint (fi (pc1-c1 roi)) (fi (pr1-r1 roi))
        pt2 = IppiPoint (fi (pc2-c1 roi)) (fi (pr2-r1 roi))
    (ippiSampleLine_8u_C1R // dst img roi) pline pt1 pt2 // checkIPP "ippiSampleLine_8u_C1R" [img]
    line <- peekArray n pline
    free pline
    return line

sampleLine32f :: ImageFloat -> Pixel -> Pixel -> [Float]
sampleLine32f (F img) (Pixel pr1 pc1) (Pixel pr2 pc2) = unsafePerformIO $ do
    let roi = vroi img
        n = max (abs(pr2-pr1+1)) (abs(pc2-pc1+1))
    pline <- mallocArray n
    let pt1 = IppiPoint (fi (pc1-c1 roi)) (fi (pr1-r1 roi))
        pt2 = IppiPoint (fi (pc2-c1 roi)) (fi (pr2-r1 roi))
    (ippiSampleLine_32f_C1R // dst img roi) pline pt1 pt2 // checkIPP "ippiSampleLine_32f_C1R" [img]
    line <- peekArray n pline
    free pline
    return line

----------------------------------------------------------------------------

-- | Creates an integral (cumulative sum) 32f image from an 8u image. Obtains a roi of the same size, but each pixel has the sum of the pixels strictly less than its position, so the first row and column contains zeroes and the last ones are not taken into account (sorry for the sentence).
integral :: ImageGray -> ImageFloat
integral im = unsafePerformIO $ do
    -- strange roi ...
    let ROI r1 r2 c1 c2 = theROI im
        roi = ROI r1 (r2-1) c1 (c2-1) -- `intersection` vroi r'
    ioIntegral_8u32f_C1R 0 (const roi) im

----------------------------------------------------------------------------

-- | Similar to integral, computing also the integral of the squares
sqrIntegral :: ImageGray -> (ImageFloat,ImageDouble)
sqrIntegral im = unsafePerformIO $ do
    -- strange roi ...
    let ROI r1 r2 c1 c2 = theROI im
        roi = ROI r1 (r2-1) c1 (c2-1) -- `intersection` vroi r'
    ioSqrIntegral_8u32f64f_C1R 0 0 (const roi) im

----------------------------------------------------------------------------

-- | Calculates standard deviation on rectangular window
rectStdDev :: Int -- ^ height of rectangle
           -> Int -- ^ width
           -> (ImageFloat,ImageDouble)  -- ^ integral images obtained from 'sqrIntegral'
           -> ImageFloat
rectStdDev h w (F imx, D imx2) = unsafePerformIO $ do
    with (IppiRect 0 0 (1+2*fromIntegral w) (1+2*fromIntegral h)) $ \prect -> do
        F r <- image (isize imx)
        (ippiRectStdDev_32f_C1R // src imx sroi // src imx2 sroi // dst r droi) prect // checkIPP "ippiRectStdDev_32f_C1R" [imx,imx2]
        return (F r {vroi = droi})
  where droi = shrink (h,w) (vroi imx)
        sroi = shift (-h,-w) droi

----------------------------------------------------------------------------

-- | Watershed segmentation.
watershed :: ImageGray -- ^ seeds
          -> ImageGray -- ^ source image
          -> ImageGray -- ^ result
watershed seed  (G im) = unsafePerformIO $ do
    let roi = vroi im
    G r <- ioCopy_8u_C1R (const roi) seed
    ps <- malloc
    (ippiSegmentWatershedGetBufferSize_8u_C1R (roiSZ roi)) ps // checkIPP "ippiSegmentWatershedGetBufferSize" []
    s <- peek ps
    buffer <- mallocBytes (fromIntegral s)
    let norm = codeNorm IppiNormL2
        flag = codeSegment "IPP_SEGMENT_DISTANCE" + codeSegment "IPP_SEGMENT_BORDER_8"
    (ippiSegmentWatershed_8u_C1IR // src im roi // dst r roi) norm flag buffer // checkIPP "ippiSegmentWatershed_8u_C1IR" [im]
    free buffer
    free ps
    return (G r {vroi = roi})

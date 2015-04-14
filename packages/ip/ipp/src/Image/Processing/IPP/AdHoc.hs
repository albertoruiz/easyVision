-----------------------------------------------------------------------------
{- |
Module      :  Image.Processing.IPP.AdHoc
Copyright   :  (c) Alberto Ruiz 2006-13
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module Image.Processing.IPP.AdHoc(
    set8u,set8u3,set32f,
    copy8u,copy8u3,copy32f,
    resize8u,resize8u3,resize32f,
    resize8uNN,resize8u3NN,resize32fNN,
    warpon8u,warpon8u3,warpon32f,
    crossCorr8u, crossCorr8u3, crossCorr32f,
    sqrDist8u, sqrDist8u3, sqrDist32f,
    twistColors,
    getChannel, rgbToYUV, yuvToUV,
    histogram, histogramN,
    floodFill8u, floodFill8uGrad,
    minmax,maxIndx32f,maxIndx8u,
    otsuThreshold,
    sum8u, sum32f,
    convolutionRow8u,convolutionColumn8u,convolution8u,
    convolutionRow32f,convolutionColumn32f,convolution32f,
    compareC32f, compare32f, copyMask32f, copyMask8u, copyMask8u3,
    canny32f
)
where

import Image.Processing.IPP.Core
import Image.Processing.IPP.Adapt
import Image.Processing.IPP.Wrappers
import Image.Processing.IPP.Auto
import Foreign.Marshal
import Control.Monad(when)
import Data.ByteString.Internal as B
import Foreign.Ptr(plusPtr,castPtr)
import Foreign.Storable(peek)


-- | Writes into a existing image a desired value in a specified roi.
set32f :: Float
       -> ROI
       -> Image Float
       -> IO ()
set32f v dr im = when (roiArea r > 0) $ do
    ippiSet_32f_C1R v // dst (setROI r im) // checkIPP "set32f"
  where
    r = intersection dr (roi im)

-- | Writes into a existing image a desired value in a specified roi.
set8u :: Word8
       -> ROI
       -> Image Word8
       -> IO ()
set8u v dr im = when (roiArea r > 0) $ do
    ippiSet_8u_C1R v // dst (setROI r im) // checkIPP "set8u"
  where
    r = intersection dr (roi im)


-- | Writes into a existing image a desired value in a specified roi.
set8u3 :: Word24
       -> ROI
       -> Image Word24
       -> IO ()
set8u3 (Word24 r g b) dr im = when (roiArea ar > 0) $ do
    v <- mallocArray 3
    pokeArray v [r,g,b]
    ippiSet_8u_C3R v // dst (setROI ar im) // checkIPP "set8u3"
    free v
  where
    ar = intersection dr (roi im)

--------------------------------------------------------------------------------

copyg :: String -> Src p (Dst p (IO CInt))
      -> Image p -> Image p -> Pixel -> IO ()

copyg msg f s d (Pixel r c) | r < r0 = copyg msg f (setROI rs s) d (Pixel r0 c)
  where
    ROI r1 r2 c1 c2 = roi s
    ROI r0 _  _  _  = roi d
    rs = ROI (r1-r+r0) r2 c1 c2

copyg msg f s d (Pixel r c) | c < c0 = copyg msg f (setROI rs s) d (Pixel r c0)
  where
    ROI r1 r2 c1 c2 = roi s
    ROI _  _  c0 _  = roi d
    rs = ROI r1 r2 (c1-c+c0) c2

copyg msg f s d pix = do
    let r = intersection (roi d) (roi s `roiAt` pix)
    when (roiArea r > 0) $ withImage s $ do
        f // src s (roi s) // dst (setROI r d) // checkIPP msg
    return ()

copy8u :: Image Word8 -> Image Word8 -> Pixel -> IO ()
copy8u = copyg "copy8u" ippiCopy_8u_C1R

copy8u3 :: Image Word24 -> Image Word24 -> Pixel -> IO ()
copy8u3 = copyg "copy8u3" ippiCopy_8u_C3R

copy32f :: Image Float -> Image Float -> Pixel -> IO ()
copy32f = copyg "copy32f" ippiCopy_32f_C1R

--------------------------------------------------------------------------------

resizeg :: Storable p => String -> RawImage p (RawImage p (IO CInt))
        -> Size -> Image p -> Image p
resizeg msg f s im = unsafePerformIO $ do
    when (roiArea (fullROI s) <=0) $ error $ "resize result " ++ show s
    r <- newImage undefined s
    withImage im $ checkIPP msg $ do
        f `appI` im `appI` r
    return r

resize32f :: Size -> Image Float -> Image Float
resize32f = resizeg "resize32f" c_resize32f

resize8u :: Size -> Image Word8 -> Image Word8
resize8u = resizeg "resize8u" c_resize8u

resize8u3 :: Size -> Image Word24 -> Image Word24
resize8u3 = resizeg "resize8u3" c_resize8u3

resize32fNN :: Size -> Image Float -> Image Float
resize32fNN = resizeg "resize32fNN" c_resize32f_NN

resize8uNN :: Size -> Image Word8 -> Image Word8
resize8uNN = resizeg "resize8uNN" c_resize8u_NN

resize8u3NN :: Size -> Image Word24 -> Image Word24
resize8u3NN = resizeg "resize8u3NN" c_resize8u3_NN

--------------------------------------------------------------------------------

warpong :: Storable p => String -> (Ptr Double -> CInt -> CInt -> RawImage p (RawImage p (IO CInt)))
        -> [[Double]] -> Image p -> Image p -> IO ()
warpong msg f h s d = do
    ph <- newArray (concat h)
    withImage s $ do
        f ph (fi r) (fi c) `appI` s `appI` d // checkIPP msg
    free ph
  where
    Size r c = size s

warpon8u  = warpong "warpOn8u"  warpPerspective8u
warpon8u3 = warpong "warpOn8u3" warpPerspective8u3
warpon32f = warpong "warpOn32f" warpPerspective32f


--------------------------------------------------------------------------------

-- | extracts a given channel of a 8uC3 image into a 8uC1 image
getChannel :: Int -> Image Word24 -> Image Word8
getChannel c im = unsafePerformIO $ do
    when (c<0 || c>2) $ error $ "getChannel " ++ show c 
    res' <- newImage (undefined::Word8) (size im)
    let res = setROI (roi im) res'
        B.PS fp o l = bytes im
        im' = im { bytes = B.PS fp (o+c) (l-c) } :: Image Word8
    withImage im' $ do
        ippiCopy_8u_C3C1R // src im' (roi im) // dst res // checkIPP "ippiCopy_8u_C3C1R"
    return res

-- | Creates  a YUV420 image (typically generated by a MPlayer camera) from a 8uC3R RGB image. TODO : only ROI
rgbToYUV :: ImageRGB  -- ^ input image
         -> ImageYUV  -- ^ result
rgbToYUV im = unsafePerformIO $ do
    when (roi im /= fullROI (size im)) $ error $ "rgbToYUV "++ show (roi im)
    res' <- newImage (undefined :: Word16) (size im)
    let res = res' {roi = roi im}
    pdst  <- mallocArray 3
    let Size h w = size im
        ps = starting res
    pokeArray pdst [ps, ps `plusPtr` (h*w), ps `plusPtr` (h*w + h*w `div` 4)]
    pstep <- mallocArray 3
    pokeArray pstep (map fi [w, w`div`2, w`div`2])
    withImage im $ do
        ippiRGBToYUV420_8u_C3P3R (starting im) (step im) (castPtr pdst) pstep (roiSZ (roi res)) // checkIPP "rgbToYUV"
    free pdst
    free pstep
    return res


-- | Extract U and V channels (half size)
yuvToUV :: ImageYUV
       -> (Image Word8, Image Word8)
yuvToUV im = (u,v)
  where
    Size r c = size im
    r' = r `div` 2
    c' = c `div` 2
    uv = im { size = Size r' c'
            , step = c'
            , roi = roiDiv 2 (roi im)
            } :: Image Word8
    B.PS fp o l = bytes im
    u = uv { bytes = B.PS fp (o+r*c) (l-r*c) }
    v = uv { bytes = B.PS fp (o+r*c + r'*c') (l-r*c-r'*c') }



{-

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
yuvToGray (Y im) = G im {layers = 1, itype = Gray, step = width (isize im), jump = width (isize im)}


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


-}
------------------------------------------------------------------

-- | Fills (as a side effect) a connected component in the image, starting at the seed pixel. It returns
-- the enclosing ROI, area and value. This is the 8con version.
floodFill8u :: Image I8u -> Pixel -> I8u -> IO (ROI, Int, I8u)
floodFill8u im (Pixel r c) val = do
    let droi@(ROI r1 _ c1 _) = roi im
    pregion <- malloc
    pbufsize <- malloc
    (ippiFloodFillGetSize (roiSZ droi)) pbufsize // checkIPP "ippiFloodFillGetSize"
    bufsize <- peek pbufsize
    buf <- mallocBytes (fromIntegral bufsize)
    free pbufsize
    withImage im $ do
        (ippiFloodFill_8Con_8u_C1IR // dst im) (IppiPoint (fi $ c-c1) (fi $ r-r1)) val pregion buf // checkIPP "ippiFloodFill_8Con_8u_C1IR"
    free buf
    IppiConnectedComp area value0 _ _ (IppiRect x y w h) <- peek pregion
    free pregion
    return (ROI (r1+ ti y) (r1+ti y+ti h-1) (c1+ti x) (c1+ti x+ti w-1), round area, round value0)



-- | Fills (as a side effect) a connected component in the image, starting at the seed pixel.
-- This version admits a lower and higher difference in the pixel values.
-- It returns the enclosing ROI, area and value. This is the 8con version.
floodFill8uGrad :: Image I8u -> Pixel -> I8u -> I8u -> I8u-> IO (ROI, Int, I8u)
floodFill8uGrad im (Pixel r c) dmin dmax val = do
    let droi@(ROI r1 _ c1 _) = roi im
    pregion <- malloc
    pbufsize <- malloc
    ippiFloodFillGetSize (roiSZ droi) pbufsize // checkIPP "ippiFloodFillGetSize"
    bufsize <- peek pbufsize
    buf <- mallocBytes (fromIntegral bufsize)
    free pbufsize
    withImage im $ do
        (ippiFloodFill_Grad8Con_8u_C1IR // dst im) (IppiPoint (fi $ c-c1) (fi $ r-r1)) val dmin dmax pregion buf // checkIPP "ippiFloodFill_Grad8Con_8u_C1IR"
    free buf
    IppiConnectedComp area value0 _ _ (IppiRect x y w h) <- peek pregion
    free pregion
    return (ROI (r1+ ti y) (r1+ti y+ti h-1) (c1+ti x) (c1+ti x+ti w-1), round area, round value0)


----------------------------------------------------------------


-- | Returns the minimum and maximum value in an image32f
minmax :: Image Float -> (Float,Float)
minmax im = unsafePerformIO $ do
    mn <- malloc
    mx <- malloc
    withImage im $ do
        (ippiMinMax_32f_C1R // dst im) mn mx // checkIPP "minmax"
    a <- peek mn
    b <- peek mx
    free mn
    free mx
    return (a,b)


maxIndx32f :: Image Float -> (Float,Pixel)
maxIndx32f im = unsafePerformIO $ do
    mx <- malloc
    px <- malloc
    py <- malloc
    withImage im $ do
        (ippiMaxIndx_32f_C1R // dst im) mx px py // checkIPP "maxIndx"
    v <- peek mx
    x <- peek px
    y <- peek py
    free mx
    free px
    free py
    let ROI r1 _ c1 _ = roi im
    return (v,Pixel (r1+fromIntegral y) (c1+fromIntegral x))


maxIndx8u :: Image I8u -> (I8u,Pixel)
maxIndx8u im = unsafePerformIO $ do
    mx <- malloc
    px <- malloc
    py <- malloc
    withImage im $ do
        (ippiMaxIndx_8u_C1R // dst im) mx px py // checkIPP "maxIndx8u"
    v <- peek mx
    x <- peek px
    y <- peek py
    free mx
    free px
    free py
    let ROI r1 _ c1 _ = roi im
    return (v,Pixel (r1 + fromIntegral y) (c1 + fromIntegral x))


----------------------------------------------------------------------

compareC32f :: Float -> IppCmp -> Image Float -> Image I8u
compareC32f v cmp im = unsafePerformIO $ do
    r <- setROI (roi im) <$> newImage undefined (size im)
    withImage im $ checkIPP "compareC32f" $ do
        ((ippiCompareC_32f_C1R // src im (roi im)) v // dst r) (codeCmp cmp)
    return r


-- | The result is the pixelswise comparation of the two source images.
compare32f :: IppCmp -> Image Float -> Image Float -> Image I8u
compare32f cmp im1 im2 = unsafePerformIO $ do
    let droi = intersection (roi im1) (roi im2)
    r <- setROI droi <$> newImage undefined (size im1)
    withImage im1 $ withImage im2 $ do
        (ippiCompare_32f_C1R // src im1 droi // src im2 droi // dst r) (codeCmp cmp) // checkIPP "compare32f"
    return r



-- FIXME generic copyMask

-- | Creates a copy of the source image only on corresponding pixels in which mask=255
copyMask32f :: Image Float   -- ^ base image
            -> Image Float   -- ^ source image
            -> Image I8u     -- ^ mask image
            -> Image Float   -- ^ result
copyMask32f base im mask = unsafePerformIO $ do
    let droi = intersection (roi im) (roi mask) `intersection` (roi base)
    r <- cloneImage base
    withImage im $ withImage mask $ do
        ippiCopy_32f_C1MR // src im droi // dst (setROI droi r) // src mask droi // checkIPP "copyMask32f"
    return r



-- | Creates a copy of the source image only on corresponding pixels in which mask=255
copyMask8u  :: Image I8u   -- ^ base image
            -> Image I8u    -- ^ source image
            -> Image I8u    -- ^ mask image
            -> Image I8u    -- ^ result
copyMask8u base im mask = unsafePerformIO $ do
    let droi = intersection (roi im) (roi mask) `intersection` (roi base)
    r <- cloneImage base
    withImage im $ withImage mask $ do
        ippiCopy_8u_C1MR // src im droi // dst (setROI droi r) // src mask droi // checkIPP "copyMask8u"
    return r


-- | Creates a copy of the source image only on corresponding pixels in which mask=255
copyMask8u3  :: Image I8u3    -- ^ base image
             -> Image I8u3    -- ^ source image
             -> Image I8u     -- ^ mask image
             -> Image I8u3    -- ^ result
copyMask8u3 base im mask = unsafePerformIO $ do
    let droi = intersection (roi im) (roi mask) `intersection` (roi base)
    r <- cloneImage base
    withImage im $ withImage mask $ do
        ippiCopy_8u_C3MR // src im droi // dst (setROI droi r) // src mask droi // checkIPP "copyMask8u3"
    return r



-- | Sum of all pixels in the roi a 8u image
sum8u :: Image I8u -> Double
sum8u im = unsafePerformIO $ do
    pf <- malloc
    withImage im $ do
        (ippiSum_8u_C1R // dst im) pf // checkIPP "sum8u"
    r <- peek pf
    free pf
    return r

sum32f :: Image Float -> Double
sum32f im = unsafePerformIO $ do
    pf <- malloc
    withImage im $ do
        (ippiSum_32f_C1R // dst im) pf (codeAlgHint AlgHintNone) // checkIPP "sum32f"
    r <- peek pf
    free pf
    return r




otsuThreshold :: Image I8u -> I8u
otsuThreshold im = unsafePerformIO $ do
    pf <- malloc
    withImage im $ do
        (ippiComputeThreshold_Otsu_8u_C1R // dst im) pf // checkIPP "otsuThreshold"
    r <- peek pf
    free pf
    return r

{-

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

-}

-- | Canny's edge detector.
canny32f :: (Image Float,Image Float) -- ^ image gradient (dx,dy)
      -> (Float,Float)           -- ^ low and high threshold
      -> Image I8u               -- ^ resulting image
canny32f (dx, dy) (l,h) = unsafePerformIO $ do
    let droi = intersection (roi dx) (roi dy)
    r <- setROI droi <$> newImage undefined (size dx)
    ps <- malloc

    (ippiCannyGetSize (roiSZ droi)) ps // checkIPP "ippiCannyGetSize"
    s <- peek ps
    buffer <- mallocBytes (fromIntegral s)
    withImage dx $ withImage dy $ do
        (ippiCanny_32f8u_C1R // src dx droi // src dy droi // dst r) l h buffer
                             // checkIPP "ippiCanny_32f8u_C1R"
    free buffer
    free ps
    return r



--------------------------------------------------------------------------------

-- | Histogram of a 8u image. For instance, @histogram [0,64 .. 256] g@ computes an histogram with four bins equally spaced in the image range.
histogram :: [Int] -- ^ bin bounds
          -> Image I8u -- ^ source image
          -> [Int]     -- result
histogram bins im = unsafePerformIO $ do
    let n = length bins
    pbins <- newArray (map fromIntegral bins)
    pr <- mallocArray n
    withImage im $ do
        (ippiHistogramRange_8u_C1R // dst im) pr pbins n // checkIPP "histogram"
    r <- peekArray (n-1) pr
    free pbins
    free pr
    return (map fromIntegral r)

-- normalized histogram
histogramN :: [Int] -> Image Word8 -> [Double]
histogramN bins im = map ((*sc).fromIntegral) h where
    h = histogram bins im
    ROI r1 r2 c1 c2 = roi im
    sc = (1.0::Double) / fromIntegral ((r2-r1+1)*(c2-c1+1))

-----------------------------------------------------------------------------------

convolution32f :: [[Float]] -> ImageFloat -> ImageFloat
convolution32f mask img = unsafePerformIO $ do
    pKernel <- newArray (concat mask)
    let r = fi $ length mask
        rm = r `div` 2
        c = fi $ length (head mask)
        cm = c `div` 2
    res <- ioFilter_32f_C1R pKernel (IppiSize r c) (IppiPoint cm rm) (shrink (ti rm,ti cm)) img
    free pKernel
    return res

convolution8u :: [[Int]] -> Int -> ImageGray -> ImageGray
convolution8u mask divisor img = unsafePerformIO $ do
    pKernel <- newArray (map fromIntegral $ concat mask)
    let r = fi $ length mask
        rm = r `div` 2
        c = fi $ length (head mask)
        cm = c `div` 2
    res <- ioFilter_8u_C1R pKernel (IppiSize r c) (IppiPoint cm rm) divisor (shrink (ti rm,ti cm)) img
    free pKernel
    return res

convolutionColumn32f :: [Float] -> ImageFloat -> ImageFloat
convolutionColumn32f mask img = unsafePerformIO $ do
    pKernel <- newArray mask
    let r = length mask
        rm = r `div` 2
    res <- ioFilterColumn_32f_C1R pKernel r rm (shrink (rm,0)) img
    free pKernel
    return res

convolutionColumn8u :: [Int] -> Int -> ImageGray -> ImageGray
convolutionColumn8u mask divisor img = unsafePerformIO $ do
    pKernel <- newArray (map fromIntegral mask)
    let r = length mask
        rm = r `div` 2
    res <- ioFilterColumn_8u_C1R pKernel r rm divisor (shrink (rm,0)) img
    free pKernel
    return res

convolutionRow32f :: [Float] -> ImageFloat -> ImageFloat
convolutionRow32f mask img = unsafePerformIO $ do
    pKernel <- newArray mask
    let r = length mask
        rm = r `div` 2
    res <- ioFilterRow_32f_C1R pKernel r rm (shrink (0,rm)) img
    free pKernel
    return res

convolutionRow8u :: [Int] -> Int -> ImageGray -> ImageGray
convolutionRow8u mask divisor img = unsafePerformIO $ do
    pKernel <- newArray (map fromIntegral mask)
    let r = length mask
        rm = r `div` 2
    res <- ioFilterRow_8u_C1R pKernel r rm divisor (shrink (0,rm)) img
    free pKernel
    return res

{-

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

-- | Watershed segmentation basic routine which overwrites the seeds image. See the higher level version 'watershed'.
watershed8u :: ImageGray -- ^ source image
            -> ImageGray -- ^ seeds
            -> IO ()
watershed8u (G im) (G seed) = do
    let roi = vroi im
    ps <- malloc
    (ippiSegmentWatershedGetBufferSize_8u_C1R (roiSZ roi)) ps // checkIPP "ippiSegmentWatershedGetBufferSize" []
    s <- peek ps
    buffer <- mallocBytes (fromIntegral s)
    let norm = codeNorm IppiNormL2
        flag = codeSegment "IPP_SEGMENT_DISTANCE" + codeSegment "IPP_SEGMENT_BORDER_8"
    (ippiSegmentWatershed_8u_C1IR // src im roi // dst seed roi) norm flag buffer // checkIPP "ippiSegmentWatershed_8u_C1IR" [im]
    free buffer
    free ps

----------------------

-- | watershed segmentation
watershed :: ImageGray      -- ^ source image
          -> [(Pixel,Int)]  -- ^ seeds
          -> ImageGray      -- ^ result
watershed im seeds = unsafePerformIO $ do
    G s <- image (size im)
    set8u 0 (vroi s) (G s)
    let put (Pixel r c, v) = setValue s (fromIntegral v :: CUChar) r c
    mapM_ put seeds
    watershed8u im (G s)
    return $ G (s {vroi = theROI im})

-----------------------

newtype LookupMap = LookupMap (ImageFloat, ImageFloat, Ptr CUChar)

-- | Creates a lookup map to correct radial distortion (parameters in pixel coordinates)
undistortMapRaw :: Size  -- ^ image size
                -> Float -- ^ fx
                -> Float -- ^ fy
                -> Float -- ^ cx
                -> Float -- ^ cy
                -> Float -- ^ k1
                -> Float -- ^ k2
                -> Float -- ^ p1
                -> Float -- ^ p2
                -> IO LookupMap
undistortMapRaw sz fx fy cx cy k1 k2 p1 p2 = do
    ps <- malloc
    let r = fi (height sz)
    let c = fi (width sz)
        isz = IppiSize r c
    ippiUndistortGetSize isz ps // checkIPP "ippiUndistortGetSize" []
    s <- peek ps
    buffer <- mallocBytes (fromIntegral s)
    F xmap <- image sz
    F ymap <- image sz
    with isz $ \pisz ->
        (ippiCreateMapCameraUndistort_32f_C1Rx // src xmap (vroi xmap) // src ymap (vroi ymap)) pisz fx fy cx cy k1 k2 p1 p2 buffer // checkIPP "ippiCreateMapCameraUndistort_32f_C1Rx" [xmap, ymap]
    return $ LookupMap (F xmap, F ymap, buffer)

-- | Creates a lookup map to correct radial distortion (for diag f f 1 cameras)
undistortMap :: Size    -- ^ image size
             -> Float   -- ^ f camera parameter in normalized coordinates (e.g. 2.0)
             -> Float   -- ^ k parameter for distortion radial (quadratic).
             -> IO LookupMap  -- ^ result expected by 'remap'
undistortMap sz f k = undistortMapRaw sz fp fp (fromIntegral w / 2) (fromIntegral h / 2) k 0 0 0
        where Size h w = sz
              fp = f * fromIntegral w / 2

remap8u (F xmap, F ymap, buffer) mode (G im) = unsafePerformIO $ do
    let sz = isize im
        Size h w = sz
        rect = IppiRect 0 0 (fi w) (fi h)
    G r <- image sz
    with rect $ \prect ->
        (ippiRemap_8u_C1R (castPtr $ ptr im) (roiSZ $ vroi im) (step im) prect (castPtr $ ptr xmap) (step xmap) (castPtr $ ptr ymap) (step ymap) // dst r (vroi r)) (interCode mode) // checkIPP "ippiRemap_8u_C1R" [im,xmap,ymap]
    return (G r)

remapRGB (F xmap, F ymap, buffer) mode (C im) = unsafePerformIO $ do
    let sz = isize im
        Size h w = sz
        rect = IppiRect 0 0 (fi w) (fi h)
    C r <- image sz
    with rect $ \prect ->
        (ippiRemap_8u_C3R (castPtr $ ptr im) (roiSZ $ vroi im) (step im) prect (castPtr $ ptr xmap) (step xmap) (castPtr $ ptr ymap) (step ymap) // dst r (vroi r)) (interCode mode) // checkIPP "ippiRemap_8u_C1R" [im,xmap,ymap]
    return (C r)

remap32f (F xmap, F ymap, buffer) mode (F im) = unsafePerformIO $ do
    let sz = isize im
        Size h w = sz
        rect = IppiRect 0 0 (fi w) (fi h)
    F r <- image sz
    with rect $ \prect ->
        (ippiRemap_32f_C1R (castPtr $ ptr im) (roiSZ $ vroi im) (step im) prect (castPtr $ ptr xmap) (step xmap) (castPtr $ ptr ymap) (step ymap) // dst r (vroi r)) (interCode mode) // checkIPP "ippiRemap_8u_C1R" [im,xmap,ymap]
    return (F r)


--------------------------------------------------------------------------------

data InpaintMethod = InpaintTelea
                   | InpaintNavierStokes
                   deriving (Enum)

-- | inpainting
inpainting :: Float -- ^ radius
           -> InpaintMethod -- ^ algorithm
           -> ImageGray  -- ^ source
           -> ImageGray  -- ^ mask
           -> ImageFloat  -- ^ distance
           -> ImageGray  -- ^ result
inpainting rad meth (G s) (G m) (F d) = unsafePerformIO $ do
    let roi = foldl1' intersection (map vroi [s,m,d])
    G r' <- image (isize s)
    let r = r' {vroi = roi}
    (auxInpainting_8u_C1R
           rad (fromEnum meth)
           // src s roi
           // src m roi
           // src d roi
           // src r roi
           ) (r1 roi) (r2 roi) (c1 roi) (c2 roi)
           // checkIPP "auxInpainting_8u_C1R" [s,m,d]
    return (G r)

--------------------------------------------------------------------------------

-- | Calculates distance transform to closest zero pixel for all non-zero pixels of source image using fast marching method.
fastMarching :: Float     -- ^ radius
             -> ImageGray
             -> ImageFloat
fastMarching rad (G s) = unsafePerformIO $ do
    F r <- image (isize s)
    ps <- malloc
    let roi = vroi s
    (ippiFastMarchingGetBufferSize_8u32f_C1R (roiSZ roi)) ps // checkIPP "fastMarchingGetSize" []
    bsz <- peek ps
    buffer <- mallocBytes (fromIntegral bsz)
    (ippiFastMarching_8u32f_C1R // src s roi // dst r roi) rad buffer
                         // checkIPP "ippiFastMarching_8u32f_C1R" [s]
    free buffer
    free ps
    return (F r {vroi = roi})

--------------------------------------------------------------------------------

-}

ioCrossCorrValid_NormLevel_32f_C1R  = {-# SCC "ippiCrossCorrValid_NormLevel_32f_C1R" #-} special_2  ippiCrossCorrValid_NormLevel_32f_C1R "ippiCrossCorrValid_NormLevel_32f_C1R"

ioSqrDistanceValid_Norm_32f_C1R  = {-# SCC "ippiSqrDistanceValid_Norm_32f_C1R" #-} special_2 ippiSqrDistanceValid_Norm_32f_C1R "ippiSqrDistanceValid_Norm_32f_C1R"

imgAsR2b roifun im1 im2 = do
    r <- newImage (undefined::Float) (size im1)
    return r {roi = roifun (roi im1) (roi im2)}

cr2b :: Dst p (Dst p (Src Float (IO CInt))) -> String -> Image p -> Image p -> Image Float -> IO()
cr2b f msg im1 im2 r = withImage im1 $ withImage im2 $ do
    f // dst im1 // dst im2 // src r (roi r) // checkIPP msg


special_2 f msg rf im1 im2 = do
    r <- imgAsR2b rf im1 im2
    cr2b f msg im1 im2 r
    return r


ccsd f temp imag = unsafePerformIO $ f g imag temp
  where
    g iroi roimask = ROI r1' r2' c1' c2'
      where
        Size h w = roiSize roimask
        ROI r1 r2 c1 c2 = iroi
        r1' = r1 + (h-1) `div` 2
        r2' = r1'+r2-r1-h+1
        c1' = c1 + (w-1) `div` 2
        c2' = c1'+c2-c1-w+1


crossCorr32f :: Image Float -> Image Float -> Image Float
crossCorr32f = ccsd ioCrossCorrValid_NormLevel_32f_C1R

sqrDist32f :: Image Float -> Image Float -> Image Float
sqrDist32f = ccsd ioSqrDistanceValid_Norm_32f_C1R

--------------------------------------------------------------------------------



ioCrossCorrValid_NormLevel_8u32f_C3R  = {-# SCC "ippiCrossCorrValid_NormLevel_8u32f_C13R" #-} special_2_8u32f_C3R ippiCrossCorrValid_NormLevel_8u32f_C3R "ippiCrossCorrValid_NormLevel_8u32f_C3R"

ioSqrDistanceValid_Norm_8u32f_C3R  = {-# SCC "ippiSqrDistanceValid_Norm_8u32f_C3R" #-} special_2_8u32f_C3R  ippiSqrDistanceValid_Norm_8u32f_C3R "ippiSqrDistanceValid_Norm_8u32f_C3R"


special_2_8u32f_C3R f msg rf im1 im2 = do
    let Size r c = size im1
        ROI r1 r2 c1 c2 = rf (roi im1) (roi im2)
    r0 <- newImage (undefined::Float) (Size r (3*c))
    let res = r0 {roi = ROI r1 r2 (c1*3) (c2*3+2)}
    cr2b f msg im1 im2 res
    return res


crossCorr8u3 :: Image Word24 -> Image Word24 -> Image Float
crossCorr8u3 = ccsd ioCrossCorrValid_NormLevel_8u32f_C3R

sqrDist8u3 :: Image Word24 -> Image Word24 -> Image Float
sqrDist8u3 = ccsd ioSqrDistanceValid_Norm_8u32f_C3R

--------------------------------------------------------------------------------



ioCrossCorrValid_NormLevel_8u32f_C1R  = {-# SCC "ippiCrossCorrValid_NormLevel_8u32f_C1R" #-} special_2 ippiCrossCorrValid_NormLevel_8u32f_C3R "ippiCrossCorrValid_NormLevel_8u32f_C1R"

ioSqrDistanceValid_Norm_8u32f_C1R  = {-# SCC "ippiSqrDistanceValid_Norm_8u32f_C1R" #-} special_2  ippiSqrDistanceValid_Norm_8u32f_C3R "ippiSqrDistanceValid_Norm_8u32f_C1R"


crossCorr8u :: Image Word8 -> Image Word8 -> ImageFloat
crossCorr8u = ccsd ioCrossCorrValid_NormLevel_8u32f_C1R

sqrDist8u :: Image Word8 -> Image Word8 -> ImageFloat
sqrDist8u = ccsd ioSqrDistanceValid_Norm_8u32f_C1R


--------------------------------------------------------------------------------


twistColors :: [[Float]] -> Image Word24 -> Image Word24
twistColors twist img = unsafePerformIO $ do
    pTwist <- newArray (concat twist)
    r <- ioColorTwist32f_8u_C3R pTwist id img
    free pTwist
    return r


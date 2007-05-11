-- demo of some functions
-- left click for a menu, right click and drag to select the region of interest
-- examples:
-- ./demo penguin.dv
-- ./demo tv://

import EasyVision
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)
import qualified Data.Map as Map
import Foreign.C.Types
import Foreign
import GSL hiding (size,norm)
import Vision


import ImagProc.Ipp.Core
import Debug.Trace

debug x = trace (show x) x

------------------------------------------------------------

main = do
    args <- getArgs

    let opts = Map.fromList $ zip args (tail args)

    let sz = if Map.member "--size" opts
                 then mpSize $ read $ Map.findWithDefault "20" "--size" opts
                 else Size (read $ Map.findWithDefault "480" "--rows" opts)
                           (read $ Map.findWithDefault "640" "--cols" opts)

    (cam, ctrl)  <- mplayer (args!!0) sz >>= withPause

    state <- prepare "RGB"

    o <- createParameters state [("umbral",realParam 0.5 0 1),
                                 ("umbral2",intParam 128 1 255),
                                 ("area",percent 5),
                                 ("h",percent 20),
                                 ("fracpix",realParam (1.5) 0 10),
                                 ("smooth",intParam 3 0 10),
                                 ("smooth2",intParam 0 0 10)]

    addWindow "demo" sz Nothing (const (kbdcam ctrl)) state
                                   ---- or undefined ---

    getRoi <- roiControl (ROI 0 (height sz) (width sz`div`2) (width sz)) (kbdcam ctrl)

    let mode m = MenuEntry m $ modifyIORef state $ \s -> s {ust = m}

    attachMenu LeftButton $ Menu $ map mode
        ["RGB","Gray","Red","Green","Blue","U","V"
        , "Median","Gaussian","Laplacian","HighPass","Histogram"
        ,"Integral","Threshold","FloodFill","Contours","Contours W","Distance", "Hessian"
        ,"Corners", "Features", "Segments", "Canny", "DCT", "FFT", "Test 1", "Test 2"]

    fft <- genFFT 8 8 DivFwdByN AlgHintFast

    launch state (worker cam o getRoi fft)

-----------------------------------------------------------------

k = 1/(640*480*128)

worker cam param getRoi fft inWindow op = do

    th <- getParam param "umbral"
    th2' <- getParam param "umbral2" ::IO Int
    let th2 = fromIntegral th2'
    ph <- getParam param "h" :: IO Int
    let h1 = fromIntegral ph / 100
    smooth <- getParam param "smooth"
    smooth2 <- getParam param "smooth2" :: IO Int
    area <- getParam param "area" :: IO Int
    fracpix <- getParam param "fracpix"

    inWindow "demo" $ case op of

        "RGB"  ->
             cam >>= yuvToRGB >>= drawImage
        "Gray" ->
             cam >>= yuvToGray >>= drawImage
        "Red" -> do
             (r,_,_) <- cam >>= yuvToRGB_P
             drawImage r
        "Green" -> do
             (_,g,_) <- cam >>= yuvToRGB_P
             drawImage g
        "Blue" -> do
             (_,_,b) <- cam >>= yuvToRGB_P
             drawImage b
        "U" -> do
             (_,u,_) <- cam >>= yuvToYUV_P
             drawImage u
        "V" -> do
             (_,_,v) <- cam >>= yuvToYUV_P
             drawImage v
        "Integral" ->
             cam >>= yuvToGray >>= integral >>= scale32f k >>= drawImage
        "Threshold" ->
             cam >>=
             yuvToGray >>=
             scale8u32f 0 1 >>=
             thresholdVal32f th 0 IppCmpLess >>=
             thresholdVal32f th 1 IppCmpGreater >>=
             drawImage
        "FloodFill" -> do
             im' <- cam >>= yuvToGray >>= smooth2 `times` median Mask3x3 >>= binarize8u th2 True >>= copy8u
             let im = purifyWith (set8u 0) $ return $ modifyROI (shrink (5,5)) im'
                 (Size h w) = size im
                 start = (Pixel (h `div`2 ) (w `div`2))
             --(r,a,v) <- floodFill8uGrad im start 5 5 128
             (r,a,v) <- floodFill8u im start 128
             --(r,a) <- floodFill8uGrad im (snd $ maxIndx8u im) th2 th2 128
             drawImage (modifyROI (const r) im)
             pointCoordinates (size im)
             setColor 1 0 0
             text2D 0.9 0 (show (a,v))
        "Contours" -> do
             orig <- cam
             im <-yuvToGray orig >>= smooth2 `times` median Mask3x3 
             --drawImage orig
             yuvToGray orig >>= drawImage
             pixelCoordinates (size im)
             setColor 1 0 0
             lineWidth $= 3
             let (Size h w) = size im
                 pixarea = h*w*area`div`1000
                 redu = douglasPeuckerClosed fracpix
                 cs1 = map (redu.fst3) $ contours 100 pixarea th2 True im
                 cs2 = map (redu.fst3) $ contours 100 pixarea th2 False im
             mapM_ (\c -> renderPrimitive LineLoop $ mapM_ vertex c) (cs1++cs2)
        "Contours W" -> do
             orig <- cam >>= yuvToGray
             im <-(smooth2 `times` median Mask3x3) orig
             drawImage orig
             pixelCoordinates (size im)
             setColor 1 0 0
             lineWidth $= 3
             let (Size h w) = size im
                 pixarea = h*w*area`div`1000
                 redu = douglasPeuckerClosed fracpix
                 cs = map (pixelsToPoints (size orig).redu.fst3) $ contours 100 pixarea th2 False im
             mapM_ (shcont im) cs
        "Distance" ->
             cam >>=
             yuvToGray >>=
             scale8u32f 0 1 >>=
             thresholdVal32f th 0 IppCmpLess >>=
             thresholdVal32f th 1 IppCmpGreater >>=
             scale32f8u 0 1 >>=
             distanceTransform [1,1.4,2.2] >>=
             scale32f (1/60) >>=
             drawImage
        "Test 1" -> do
             d <- cam >>= yuvToGray
                  >>= smooth2 `times` median Mask5x5 >>= highPass8u Mask5x5
                  >>= binarize8u th2 True >>= smooth `times` median Mask5x5
             drawImage d

        "Test 2" -> do
             d <- cam >>= yuvToGray
                  >>= median Mask5x5 >>= highPass8u Mask5x5
                  >>= binarize8u th2 False >>= smooth `times` median Mask5x5
                  >>= distanceTransform [1,1.4,2.2]
                  >>= gauss Mask5x5

             autoscale d >>= drawImage

             (mn,mx) <- minmax d
             hp <- localMax 21 (modifyROI (shrink (60,60)) d)
                   >>= thresholdVal32f (mx*h1) 0.0 IppCmpLess
                   >>= getPoints32f 500
             pixelCoordinates (size d)
             setColor 1 0 0
             pointSize $= 3
             renderPrimitive Points (mapM_ vertex hp)
             text2D 10 20 (show $ length hp)

        "Hessian" ->
             cam >>= yuvToGray >>= scale8u32f 0 1 >>=
             smooth `times` gauss Mask5x5 >>= secondOrder >>= hessian >>=
             abs32f >>= sqrt32f >>= drawImage
        "Corners" -> do
             im <- cam >>= yuvToGray >>= scale8u32f 0 1
             ips <- getCorners smooth 7 h1 500 im
             drawImage im
             pixelCoordinates (size im)
             setColor 1 0 0
             pointSize $= 3
             renderPrimitive Points (mapM_ vertex ips)
             text2D 10 20 (show $ length ips)
        "Features" -> do
             orig <- cam
             im <- yuvToGray orig >>= scale8u32f 0 1
             ips <- getSaddlePoints smooth 7 h1 500 20 10 im
             yuvToRGB orig >>= drawImage
             pointCoordinates (size im)
             setColor 1 0 0
             pointSize $= 3
             text2D 0.9 0 (show $ length ips)
             drawInterestPoints (size orig) ips
        "Canny" -> do
             roi <- getRoi
             orig <- cam
             im <- yuvToGray orig >>= scale8u32f 0 1
             F s <- (smooth `times` gauss Mask5x5) im
             gx <- sobelVert $ F s {vroi = roi `intersection` vroi s}
             gy <- sobelHoriz $ F s {vroi = roi `intersection` vroi s}
             c <- canny (gx,gy) (th/3,th) >>= scale8u32f 0 1
             copyROI32f c (theROI c) im
             drawImage im
        "Median" -> do
             orig <- cam
             im <- yuvToGray orig
             s <- (smooth `times` median Mask5x5) im
             drawImage s
        "Gaussian" -> 
             cam >>= yuvToGray >>= scale8u32f 0 1 >>= (smooth `times` gauss Mask5x5) >>= drawImage
        "Laplacian" -> 
             cam >>= yuvToGray >>= scale8u32f (-1) 1 >>= (smooth `times` gauss Mask5x5)
             >>= laplace Mask5x5
             >>= scale32f8u (-1) 1
             >>= drawImage
        "HighPass" -> do
             orig <- cam
             im <- yuvToGray orig
             s <- (smooth `times` median Mask5x5) im >>= highPass8u Mask5x5
             drawImage s
        "Histogram" -> do
             im <- cam >>= yuvToGray
             drawImage im
             pointCoordinates (size im)
             histogram [0,64 .. 256] im >>= return . show >>= text2D 0.9 0.7
        "DCT" -> do
             orig <- cam
             roi <- getRoi
             im <- yuvToGray orig >>= scale8u32f 0 1
             d <- dct (modifyROI (intersection roi) im) >>= abs32f >>= sqrt32f
             copyROI32f d (theROI d) im
             drawImage im
        "FFT" -> do
             orig <- cam
             roi <- getRoi
             let p2roi = roi `intersection`ROI (r1 roi) (r1 roi + 2^8-1) (c1 roi) (c1 roi + 2^8-1)
             im <- yuvToGray orig  >>= scale8u32f 0 1 >>= (smooth `times` gauss Mask5x5)
             d <- fft (modifyROI (const p2roi) im) >>= magnitudePack >>= powerSpectrum
             let c@(Pixel r0 c0) = cent (theROI d)
             set32f 0 (roiFrom2Pixels c c) d
             (m,Pixel rm cm) <- maxIndx d
             let ROI r1 _ c1 _ = p2roi
             print $ (rm+r1-r0,cm+c1-c0)
             print $ norm (rm+r1-r0,cm+c1-c0)
             sc <- scale32f (1/m) d
             copyROI32f sc (theROI sc) im
             drawImage im
        "Segments" -> do
             orig' <- cam >>= yuvToGray
             roi <- getRoi
             let orig = modifyROI (const roi) orig'
                 segs = segments 4 1.5 5 40 20 False orig
             drawImage orig
             setColor 1 0 0
             pointCoordinates (size orig)
             renderPrimitive Lines $ mapM_ drawSeg segs

    return op

text2D x y s = do
    rasterPos (Vertex2 x (y::GLfloat))
    renderString Helvetica12 s

cent (ROI r1 r2 c1 c2) = Pixel (r1 + (r2-r1+1)`div`2) (c1 + (c2-c1+1)`div`2)
roiFrom2Pixels (Pixel r1 c1) (Pixel r2 c2) = ROI (min r1 r2) (max r1 r2) (min c1 c2) (max c1 c2)

norm (a,b) = sqrt $ fromIntegral a^2 + fromIntegral b^2

drawSeg s = do
    vertex $ (extreme1 s)
    vertex $ (extreme2 s)


autoscale im = do
    (mn,mx) <- minmax im
    r <- scale32f8u mn mx im
    return r

fst3 (a,_,_) = a

-------------------------------------------

asSegmentsClosed ps = zipWith Segment ps' (tail ps') where ps' = ps++[head ps]

moments3aux (s,sx,sy,sx2,sy2,sxy) seg@(Segment (Point x1 y1) (Point x2 y2))
    = (s+l,
       sx+l*(x1+x2)/2,
       sy+l*(y1+y2)/2,
       sx2+l*(x1*x1 + x2*x2 + x1*x2)/3,
       sy2+l*(y1*y1 + y2*y2 + y1*y2)/3,
       sxy+l*(2*x1*y1 + x2*y1 + x1*y2 + 2*x2*y2)/6)
  where l = segmentLength seg

moments3 l = (mx,my,cxx,cyy,cxy,l1,l2,a)
    where (s,sx,sy,sx2,sy2,sxy) = (foldl moments3aux (0,0,0,0,0,0). asSegmentsClosed) l
          mx = sx/s
          my = sy/s
          cxx = sx2/s - mx*mx
          cyy = sy2/s - my*my
          cxy = sxy/s - mx*my
          ra = sqrt(cxx*cxx + 4*cxy*cxy -2*cxx*cyy + cyy*cyy)
          l1 = sqrt(0.5*(cxx+cyy+ra))
          l2 = sqrt(0.5*(cxx+cyy-ra))
          a = atan2 (2*cxy) ((cxx-cyy+ra))

shcont im c = do
    pointCoordinates (size im)
    lineWidth $= 1
    setColor 1 0 0
    renderPrimitive LineLoop $ mapM_ vertex c
    let (mx,my,_,_,_,l1,l2,a) = moments3 c
    pointSize $= 5
    lineWidth $= 2
    setColor 0 0.5 0
    renderPrimitive Points $ vertex (Point mx my)
    let t = desp (mx,my) <> rot3 (-a) <> diag (fromList [sqrt (l2/l1),1,1]) <> rot3 (a) <> desp (-mx,-my)
        p2l (Point x y) = [x,y]
        caff = ht t (map p2l c)
    renderPrimitive LineLoop $ mapM_ vertex caff


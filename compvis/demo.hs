-- demo of some functions
-- left click for a menu, right click and drag to select the region of interest
-- examples:
-- ./demo penguin.dv
-- ./demo webcam0

import EasyVision
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point,set)

------------------------------------------------------------

main = do

    sz <- findSize

    (cam, ctrl)  <- getCam 0 sz  >>= withChannels >>= withPause

    prepare

    o <- createParameters [("umbral",realParam 0.5 0 1),
                           ("umbral2",intParam 128 1 255),
                           ("area",percent 5),
                           ("h",percent 20),
                           ("fracpix",realParam (1.5) 0 10),
                           ("smooth",intParam 3 0 20),
                           ("smooth2",intParam 1 0 10),
                           ("lbpThres",intParam 0 0 100)]

    w <- evWindow "Gray" "Demo: Gray" sz Nothing (const (kbdcam ctrl))


    let d = height sz `div` 5
    evROI w $= ROI d (height sz-d) d (width sz-d)

    let mode m = MenuEntry m $ do { putW w m; windowTitle $= "Demo: "++m }

    attachMenu LeftButton $ Menu $ map mode
        ["RGB","Gray","Red","Green","Blue","H","S"
        , "Median","Gaussian","Laplacian","HighPass","Histogram"
        ,"Integral","Threshold","FloodFill","Contours","Distance", "Distance2", "Hessian"
        ,"Corners", "Features", "Segments", "Canny", "DCT", "LBP"]

    fft <- genFFT 8 8 DivFwdByN AlgHintFast

    launch (worker w cam o fft)

-----------------------------------------------------------------

worker wDemo cam param fft = do

    th <- getParam param "umbral"
    th2' <- getParam param "umbral2" ::IO Int
    let th2 = fromIntegral th2'
    ph <- getParam param "h" :: IO Int
    let h1 = fromIntegral ph / 100
    smooth <- getParam param "smooth" :: IO Int
    smooth2 <- getParam param "smooth2" :: IO Int
    area <- getParam param "area" :: IO Int
    fracpix <- getParam param "fracpix" :: IO Double
    lbpThres <- getParam param "lbpThres" :: IO Int

    op <- getW wDemo
    roi <- getROI wDemo
    orig <- cam


    let chan ch = modifyROI (const roi) (ch orig)

        gsmooth = smooth `times` gauss Mask5x5 $ float $ chan gray

        edges = canny (gx,gy) (th/3,th) where gx = (-1) .* sobelVert gsmooth
                                              gy = sobelHoriz gsmooth

    inWin wDemo $ do
        drawImage (rgb orig)
        case op of
            "RGB"   -> return () --drawImage $ chan rgb
            "Gray"  -> drawImage $ chan gray
            "Red"   -> drawImage $ chan rCh
            "Green" -> drawImage $ chan gCh
            "Blue"  -> drawImage $ chan bCh
            "H"     -> drawImage $ chan hCh
            "S"     -> drawImage $ chan sCh

            "Gaussian" -> drawImage $ gsmooth
            "Median"   -> drawImage $ smooth `times` median Mask5x5 $ chan gray
            "HighPass" -> drawImage $ highPass8u Mask5x5 $ smooth `times` median Mask5x5 $ chan gray

            "Threshold" -> drawImage $ binarize8u th2 True $ chan gray

            "Laplacian"-> drawImage $ scale32f8u (-1) 1
                                    $ laplace Mask5x5
                                    $ smooth `times` gauss Mask5x5
                                    $ scale8u32f (-1) 1
                                    $ chan gray

            "Hessian"  -> drawImage $ sqrt32f
                                    $ abs32f
                                    $ hessian
                                    $ secondOrder
                                    $ gsmooth

            "Canny"    -> drawImage $ edges

            "Distance" -> drawImage $ (1/60) .* distanceTransform [1,1.4,2.2] (notI $ edges)
            "Distance2"-> drawImage $ ((1/60) .*)
                                    $ distanceTransform [1,1.4,2.2]
                                    $ toGray
                                    $ thresholdVal32f th 1 IppCmpGreater
                                    $ thresholdVal32f th 0 IppCmpLess
                                    $ float $ chan gray

            "DCT"      -> drawImage $ sqrt32f . abs32f . dct . float $ chan gray

            "Segments" -> do let segs = segments 4 1.5 5 40 20 True (chan gray)
                             drawROI roi
                             setColor 1 1 0
                             lineWidth $= 2
                             pointCoordinates (size (gray orig))
                             renderPrimitive Lines $ mapM_ vertex segs

            "Histogram" -> do drawImage $ chan gray
                              pointCoordinates (size (gray orig))
                              setColor 1 1 0
                              lineWidth $= 2
                              renderSignal $ map (*5) $ histogramN [0..256] $ chan gray

{-
            "Integral" -> do
                let k = 1/(640*480*128)
                drawImage . (k.*) . integral . gray $ orig
                -- cam >>= yuvToGray >>= integral >>= scale32f k >>= drawImage
            "Threshold" ->
                drawImage $ thresholdVal32f th 1 IppCmpGreater
                        $ thresholdVal32f th 0 IppCmpLess
                        $ float
                        $ gray
                        $ orig
            "FloodFill" -> do
                im <- cloneClear
                    $ modifyROI (shrink (5,5))
                    $ binarize8u th2 True 
                    $ smooth2 `times` median Mask3x3 
                    $ gray
                    $ orig
    
                let (Size h w) = size im
                    start = (Pixel (h `div`2 ) (w `div`2))
                --(r,a,v) <- floodFill8uGrad im start 5 5 128
                (r,a,v) <- floodFill8u im start 128
                --(r,a) <- floodFill8uGrad im (snd $ maxIndx8u im) th2 th2 128
                drawImage (modifyROI (const r) im)
                pointCoordinates (size im)
                setColor 1 0 0
                text2D 0.9 0 (show (a,v))
            "Contours" -> do
                let im = (smooth2 `times` median Mask3x3) (gray orig)
                --drawImage orig
                drawImage (gray orig)
                pixelCoordinates (size im)
                setColor 1 0 0
                lineWidth $= 3
                let (Size h w) = size im
                    pixarea = h*w*area`div`1000
                    redu = douglasPeuckerClosed fracpix
                    cs1 = map (redu.fst3) $ contours 100 pixarea th2 True im
                    cs2 = map (redu.fst3) $ contours 100 pixarea th2 False im
                mapM_ (\c -> renderPrimitive LineLoop $ mapM_ vertex c) (cs1++cs2)
            "Distance" -> 
                drawImage
                $ ((1/60) .*)
                $ distanceTransform [1,1.4,2.2] 
                $ toGray
                $ thresholdVal32f th 1 IppCmpGreater
                $ thresholdVal32f th 0 IppCmpLess
                $ float
                $ gray
                $ orig
            "Distance2" -> do
                roi <- getROI wDemo
                let im = float (gray orig)
                drawImage im
                let F s = (smooth `times` gauss Mask5x5) im
                    gx = (-1) .* sobelVert (F s {vroi = roi `intersection` vroi s})
                    gy = sobelHoriz $ F s {vroi = roi `intersection` vroi s}
                    c = canny (gx,gy) (th/3,th)
    
                drawImage $ (1/60) .* distanceTransform [1,1.4,2.2] (notI c)
    
            "Test 1" -> do
                drawImage $ smooth `times` median Mask5x5
                        $ binarize8u th2 True
                        $ highPass8u Mask5x5
                        $ smooth2 `times` median Mask5x5
                        $ gray
                        $ orig
    
            "Test 2" -> do
                let d = gauss Mask5x5
                    $ distanceTransform [1,1.4,2.2]
                    $ smooth `times` median Mask5x5
                    $ binarize8u th2 False
                    $ highPass8u Mask5x5
                    $ median Mask5x5
                    $ gray (orig)
    
                drawImage (autoscale d)
    
                let (_,mx) = minmax d
    
                let hp = getPoints32f 500
                        $ thresholdVal32f (mx*h1) 0.0 IppCmpLess
                        $ localMax 21 
                        $ modifyROI (shrink (60,60)) 
                        $ d
    
                pixelCoordinates (size d)
                setColor 1 0 0
                pointSize $= 3
                renderPrimitive Points (mapM_ vertex hp)
                text2D 10 20 (show $ length hp)
    
            "Hessian" ->
                orig // gray // float //
                smooth `times` gauss Mask5x5 // secondOrder // hessian //
                abs32f // sqrt32f // drawImage
            "Corners" -> do
                let im = orig // gray // float
                    ips = getCorners smooth 7 h1 500 im
                drawImage im
                pixelCoordinates (size im)
                setColor 1 0 0
                pointSize $= 3
                renderPrimitive Points (mapM_ vertex ips)
                text2D 10 20 (show $ length ips)
            "Features" -> do
                let im = orig // gray // float
                    ips = getSaddlePoints smooth 7 h1 500 20 10 im
                drawImage (rgb orig)
                pointCoordinates (size im)
                setColor 1 0 0
                pointSize $= 3
                text2D 0.9 0 (show $ length ips)
                drawInterestPoints (size im) ips
            "Canny" -> do
                roi <- getROI wDemo
                let im = orig // gray // float
                    F s = (smooth `times` gauss Mask5x5) im
                    gx = (-1) .* sobelVert (F s {vroi = roi `intersection` vroi s}) -- !!! (-1)
                    gy = sobelHoriz $ F s {vroi = roi `intersection` vroi s}
                    c = canny (gx,gy) (th/3,th)
                copyROI32f (float c) (theROI c) im (theROI c)
                drawImage im
            "Median" -> do
                drawImage $ (smooth `times` median Mask5x5) $ gray (orig)
            "Gaussian" -> do
                (t,a) <- timing $
                    drawImage . (!!smooth) . iterate (gauss Mask5x5) . float . gray $ orig
                text2D 30 30 t
            "Laplacian" ->
                orig // gray // scale8u32f (-1) 1 // (smooth `times` gauss Mask5x5)
                // laplace Mask5x5 // scale32f8u (-1) 1 // drawImage
            "HighPass" -> do
                let s = orig // gray // (smooth `times` median Mask5x5) // highPass8u Mask5x5
                drawImage s
            "Histogram" -> do
                let im = orig // gray
                drawImage im
                pointCoordinates (size im)
                text2D 0.9 0.7 (show $ histogram [0,64 .. 256] im)
            "DCT" -> do
                roi <-  getROI wDemo
                let im = orig // gray // float
                    d = dct (modifyROI (intersection roi) im) // abs32f // sqrt32f
                copyROI32f d (theROI d) im (theROI d)
                drawImage im
            "FFT" -> do
                roi <-  getROI wDemo
                let p2roi = roi `intersection`ROI (r1 roi) (r1 roi + 2^8-1) (c1 roi) (c1 roi + 2^8-1)
                    im = gray orig  // scale8u32f 0 1 // (smooth `times` gauss Mask5x5)
                    d = fft (modifyROI (const p2roi) im) // magnitudePack // powerSpectrum
                let c@(Pixel r0 c0) = cent (theROI d)
                set32f 0 (roiFrom2Pixels c c) d
                let (m,_) = maxIndx d
                    sc = (1/m) .* d
                copyROI32f sc (theROI sc) im (theROI sc)
                drawImage im
            "Segments" -> do
                let segs = segments 4 1.5 5 40 20 False . modifyROI (const roi) . gray $ orig
                drawImage (gray orig)
                drawROI roi
                setColor 1 0 0
                lineWidth $= 2
                pointCoordinates (size (gray orig))
                renderPrimitive Lines $ mapM_ vertex segs
            "LBP" -> do
                let orig' = gray orig
                roi <-  getROI wDemo
                let orig = modifyROI (const roi) orig'
                    h = lbp lbpThres orig
                drawImage orig
                pointCoordinates (size orig)
                setColor 0 0 0
                renderAxes
                setColor 1 0 0
    
                let ROI r1 r2 c1 c2 = roi
                    sc = (0.1*256.0::Double) / fromIntegral ((r2-r1-1)*(c2-c1-1))
                renderSignal $ map ((*sc).fromIntegral) (tail h)
-}

cent (ROI r1 r2 c1 c2) = Pixel (r1 + (r2-r1+1)`div`2) (c1 + (c2-c1+1)`div`2)
roiFrom2Pixels (Pixel r1 c1) (Pixel r2 c2) = ROI (min r1 r2) (max r1 r2) (min c1 c2) (max c1 c2)

autoscale im = scale32f8u mn mx im where (mn,mx) = minmax im

fst3 (a,_,_) = a

shcont (Closed c) = do
    renderPrimitive LineLoop $ mapM_ vertex c

times n f = (!!n) . iterate f

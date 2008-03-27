-- experiments with 

import EasyVision
import System.Environment(getArgs)
import Graphics.UI.GLUT hiding (Size, Point, Matrix)
import Control.Monad(when)
import Numeric.GSL.Minimization
import Numeric.LinearAlgebra hiding ((.*))
import Vision
import ImagProc.Ipp.Core(intersection, shrink, roiArea)

main = do
    sz <- findSize

    --(cam,ctrl) <- getCam 0 sz >>= withChannels >>= withPause
    (cam,ctrl) <- getCam 0 sz >>= withChannels >>= addDist >>= withPause

    prepare

    w  <- evWindow Nothing "image" sz Nothing  (mouse (kbdcam ctrl))
    --wd <- evWindow Nothing "diff"  sz Nothing  (mouse (kbdcam ctrl))

    let vcam = do
        st <- getW w
        case st of
            Nothing -> do orig <- cam
                          putW w (Just orig)
            _       -> return ()
        Just i <- getW w
        return i

    --p <- panoramic (mpSize 5) 2.8 2.8 2.0 vcam cam (float.gray) (similRaw (mpSize 5))
    p <- panoramic (mpSize 5) 2.8 2.8 2.0 vcam cam (float.gray.fst) (simil (mpSize 5))

    launch (worker vcam cam w  p)

-----------------------------------------------------------------

edgedist img = (r, float edges) where
    th = 0.3
    smooth = 2
    gsmooth = smooth `times` gauss Mask5x5 $ float $ resize (mpSize 10) $ gray $ img
    edges = canny (gx,gy) (th/3,th) where gx = (-1) .* sobelVert gsmooth
                                          gy = sobelHoriz gsmooth
    r = distanceTransform [1,1.4,2.2] (notI $ edges)


addDist cam = return $ do
    orig <- cam
    let (r,e) = edgedist orig
    return (orig, (r, e))


simil smsz a h b = if ok roi then simil0 ia p roi else 10
    where ia = (fst.snd) a
          ib = (snd.snd) b
          p = warp 0 (size ia) h ib
          roi = effectiveROI (size ia) h
          ok r = r1 r >= 0 && r2 r > 50 + r1 r && c1 r >= 0 && c2 r > 50 + c1 r
          simil0 di ed roi = k * sum32f (abs32f (f di |*| f ed))
              where f = modifyROI (const roi)
                    k = recip $ fromIntegral $ roiArea (f di)



worker vcam cam w  p = do
    

{-
    let (dist,_) = edgedist b

    let imdif = dist |*| edges
        cost = sum32f imdif

    inWin w $ do
        drawImage $ gray $ orig
        text2D 20 20 (show cost)

    inWin wd $ do
        drawImage $ (1/10) .* (imdif)

-}
    inWin w $ do
        p >>= drawImage

-------------------------------------------------



-------------------------------------------------

times n f = (!!n) . iterate f

mouse _ st (MouseButton LeftButton) Down _ _ = do
    st $= Nothing

mouse def _ a b c d = def a b c d


-------------------------------------------------------

-- | Creates a panoramic view from two cameras with (nearly) common camera center.
--
-- Left click: optimize
-- z: restart from identity
-- o: end optimization
panoramic :: Size              -- ^ of monitor window
          -> Double            -- ^ focal of base camera
          -> Double            -- ^ focal of source camera
          -> Double             -- ^ focal of virtual camera
          -> IO a               -- ^ base camera
          -> IO a               -- ^ source camera
          -> (a -> ImageFloat)  -- ^ how to extract the 'true image' from the cameras
          -> (a -> Matrix Double -> a -> Double) -- ^ the cost function
          -> IO (IO ImageFloat) -- ^ resulting virtual camera
panoramic sz fi1 fi2 fo camBase camAdj sel simil = do
    wMon <- evWindow (False,[0,0,0]) "autopanoramic" sz Nothing (mouse kbdQuit)
    wWar <- warper sz "control"
    return $ do
        img0raw <- camBase
        img1raw <- camAdj

        let img0 = sel img0raw
            img1 = sel img1raw

        (rh,_) <- getW wWar
        hi <- rh

        (opt,[pi,ti,ri]) <- getW wMon
        let [pan,tilt,roll] = if opt
                                then findRot simil img0raw fi1 img1raw fi2 pi ti ri
                                else [pi,ti,ri]
            h = conjugateRotation pan tilt roll fi2 fi1
        putW wMon (opt,[pan,tilt,roll])
        let base = warp 0 (size img0) (hi<>kgen (fo/fi1)) img0
        warpOn (hi<>kgen (fo/fi1)<>h) base img1
        inWin wMon $ drawImage base
        return base
  where
    -- click to adjust
    mouse _ st (MouseButton LeftButton) Down _ _ = do
        (_,p) <- get st
        st $= (True,p)

    -- restart from identity
    mouse _ st (Char 'z') Down _ _ = do
        st $= (True,[0,0,0])

    -- end optimization
    mouse _ st (Char 'o') Down _ _ = do
        (_,p) <- get st
        st $= (False,p)

    mouse def _ a b c d = def a b c d





similRaw smsz a h b = if ok roi then simil0 ia p roi else 10
    where ia = float $ resize smsz $ gray a
          ib = float $ resize smsz $ gray b
          p = warp 0 (size ia) h ib
          roi = effectiveROI (size ia) h
          ok r = r1 r >= 0 && r2 r > 50 + r1 r && c1 r >= 0 && c2 r > 50 + c1 r
          simil0 a b roi = k * sum32f (abs32f (f a |-| f b))
              where f = modifyROI (const roi)
                    k = recip $ fromIntegral $ roiArea (f a)

effectiveROI sz h = newroi where
    r = 3/4
    trrec = pointsToPixels sz . map lp $ ht h [[1,-r], [1,r], [-1, r], [-1, -r]]
    newroi = intersection (fullroi sz)
                    ROI {r1 = (minimum $ map row trrec), c1 = (minimum $ map col trrec),
                         r2 = (maximum $ map row trrec), c2 = (maximum $ map col trrec) }

    fullroi (Size h w) = ROI {r1=0, r2=h-1, c1=0, c2=w-1}
    lp [x,y] = Point x y

conjugateRotation pan tilt rho fi fo =
        kgen fo <> rot1 tilt <> rot2 pan <> rot3 rho <> kgen (1/fi)

cost simil a fa b fb [pan, tilt, roll] = simil a h b
    where h = conjugateRotation pan tilt roll fb fa

findRot simil a fa b fb pi ti ri = fst $ minimizeNMSimplex (cost simil a fa b fb) [pi,ti,ri] [0.1*degree, 0.1*degree,0.1*degree] 1E-3 30

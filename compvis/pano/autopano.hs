-- automatic adjustment of synthetic rotation

module Main where

import EasyVision
import Graphics.UI.GLUT hiding (Matrix, Size, Point)
import Control.Monad(foldM)
import Vision
import ImagProc.Ipp.Core(intersection, shrink, validArea)
import Numeric.GSL.Minimization
import Numeric.LinearAlgebra

asFloat grab = return $ grab >>= return . scale8u32f 0 1 . yuvToGray

simil0 a b roi = k * sum32f (abs32f (f a |-| f b))
    where f = modifyROI (const roi)
          k = recip $ fromIntegral $ validArea (f a)

simil a h b = if ok roi then simil0 a p roi else 10
    where p = warpOn a h b
          roi = effectiveROI (size a) h
          ok r = r1 r >= 0 && r2 r > 50 + r1 r && c1 r >= 0 && c2 r > 50 + c1 r 


main = do
    sz  <- findSize
    cam0 <- getCam 0 sz >>= asFloat
    cam1 <- getCam 1 sz >>= asFloat

    prepare

    wImg1  <- evWindow (False,[0,0,0]) "autopanoramic" sz Nothing (mouse kbdQuit)

    w <- warper sz "rot"

    sv <- optionalSaver sz

    launch (worker cam0 cam1 wImg1 w sv)

-----------------------------------------------------------------

worker cam0 cam1 wImg1 w sv = do

    img0 <- cam0
    img1 <- cam1

    let smsz = Size 120 160
        sm0 = resize smsz img0
        sm1 = resize smsz img1

    (rh,_) <- getW w
    hi <- rh

    inWin wImg1 $ do
        (opt,[pi,ti,ri]) <- getW wImg1
        let [pan,tilt,roll] = if opt
                                then findRot sm0 sm1 pi ti ri
                                else [pi,ti,ri]
            h = conjugateRotation pan tilt roll 2.8 1
        putW wImg1 (opt,[pan,tilt,roll])
        let base = warp 0 (size img0) hi img0
        warpOn' (hi<>h) base img1 -- FIXME
        drawImage base
        sv $ grayToYUV $ scale32f8u 0 1 base

--------------------------------------

effectiveROI sz h = newroi where
    r = 3/4
    trrec = pointsToPixels sz . map lp $ ht h [[1,-r], [1,r], [-1, r], [-1, -r]]
    newroi = intersection (fullroi sz)
                    ROI {r1 = (minimum $ map row trrec), c1 = (minimum $ map col trrec),
                         r2 = (maximum $ map row trrec), c2 = (maximum $ map col trrec) }

    fullroi (Size h w) = ROI {r1=0, r2=h-1, c1=0, c2=w-1}
    lp [x,y] = Point x y

conjugateRotation pan tilt rho foc sca =
        scaling sca
        <> kgen foc
        <> rot1 tilt
        <> rot2 pan 
        <> rot3 rho 
        <> kgen (1/foc)

cost a b [pan, tilt, roll] = simil a h b
    where h = conjugateRotation pan tilt roll 2.8 1

findRot a b pi ti ri = fst $ minimize NMSimplex2 1E-3 10 [0.1*degree, 0.1*degree,0.1*degree]
                             (cost a b) [pi,ti,ri]

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
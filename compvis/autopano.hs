-- automatic adjustment of synthetic rotation

module Main where

import EasyVision
import Graphics.UI.GLUT hiding (Matrix, Size, Point)
import Control.Monad(foldM)
import Vision
import ImagProc.Ipp.Core(intersection, purifyWith, shrink, roiArea)
import Foreign
import Numeric.GSL.Minimization
import Numeric.LinearAlgebra

asFloat grab = return $ grab >>= yuvToGray >>= scale8u32f 0 1

absIm = purifyWith (set32f 0) . abs32f

sumIm = unsafePerformIO . sum32f

simil0 a b roi = k * sumIm (absIm (f a |-| f b))
    where f = modifyROI (const roi)
          k = recip $ fromIntegral $ roiArea (f a)

pasteOn base h im = unsafePerformIO $ do
    dest <- copy32f base
    warpOn h dest im
    return dest

simil a h b = if ok roi then simil0 a p roi else 10
    where p = pasteOn a h b
          roi = effectiveROI (size a) h
          ok r = r1 r >= 0 && r2 r > 50 + r1 r && c1 r >= 0 && c2 r > 50 + c1 r 


main = do
    sz  <- findSize
    cam0 <- getCam 0 sz >>= asFloat
    cam1 <- getCam 1 sz >>= asFloat

    prepare

    wImg1  <- evWindow [0,0,0] "autopanoramic" sz Nothing (mouse kbdQuit)

    w <- warper sz "rot"

    sv <- optionalSaver sz

    launch (worker cam0 cam1 wImg1 w sv)

-----------------------------------------------------------------

worker cam0 cam1 wImg1 w sv = do

    img0 <- cam0
    img1 <- cam1

    let smsz = Size 120 160
    sm0 <- resize32f smsz img0
    sm1 <- resize32f smsz img1

    (rh,_) <- getW w
    hi <- rh

    inWin wImg1 $ do
        [pi,ti,ri] <- getW wImg1
        let [pan,tilt,roll] = findRot sm0 sm1 pi ti ri
            h = conjugateRotation pan tilt roll 2.8 1
        putW wImg1 [pan,tilt,roll]
        base <- warp (size img0) hi img0
        warpOn (hi<>h) base img1
        drawImage base
        scale32f8u 0 1 base >>= grayToYUV >>= sv

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

findRot a b pi ti ri = fst $ minimizeNMSimplex (cost a b) [pi,ti,ri] [0.1*degree, 0.1*degree,0.1*degree] 1E-3 10


-- click to restart
mouse _ st (MouseButton LeftButton) Down _ _ = do
    st $= [0,0,0]

mouse def _ a b c d = def a b c d
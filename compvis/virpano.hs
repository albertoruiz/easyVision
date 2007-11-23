-- test of panoramic combinator

module Main where

import EasyVision
import Graphics.UI.GLUT hiding (Matrix, Size, Point)
import Control.Monad(foldM)
import ImagProc.Ipp.Core(intersection, purifyWith, shrink, roiArea)
import Numeric.GSL.Minimization
import Numeric.LinearAlgebra
import Foreign
import Vision


-- | Creates a panoramic view from two cameras with (nearly) common camera center. Currently the synthetic rotations are set manually, but soon...
panoramic :: Size -> IO ImageFloat -> IO ImageFloat -> IO (IO ImageFloat)
panoramic sz camBase camAdj = do
    wMon <- evWindow (False,[0,0,0]) "autopanoramic" sz Nothing (mouse kbdQuit)
    wWar <- warper sz "control"
    return $ do
        img0 <- camBase
        img1 <- camAdj

        let smsz = Size 120 160
        sm0 <- resize32f smsz img0
        sm1 <- resize32f smsz img1

        (rh,_) <- getW wWar
        hi <- rh

        (opt,[pi,ti,ri]) <- getW wMon
        let [pan,tilt,roll] = if opt
                                then findRot sm0 sm1 pi ti ri
                                else [pi,ti,ri]
            h = conjugateRotation pan tilt roll 2.8 1
        putW wMon (opt,[pan,tilt,roll])
        base <- warp (size img0) hi img0
        warpOn (hi<>h) base img1
        saved <- get currentWindow
        inWin wMon $ drawImage base
        currentWindow $= saved
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







a & b = panoramic (mpSize 5) a b


main = do
    sz  <- findSize
    n <- numCams

    cams <- mapM (flip getCam sz) [0..n-1]

    let cf k = cams!!k >>= yuvToGray >>= scale8u32f 0 1

    prepare

    cam <- foldM (&) (cf 0) (map cf [1..n-1])

    wDest  <- evWindow () "pano" (mpSize 20) Nothing (mouse (kbdQuit))

    launch (worker cam wDest)

-----------------------------------------------------------------

worker cam wDest = do
    inWin wDest $
        cam >>= drawImage

---------------------------------------------------------

mouse _ st (MouseButton LeftButton) Down _ _ = do
    st $= ()

mouse def _ a b c d = def a b c d

--------------------------------------------------------------

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

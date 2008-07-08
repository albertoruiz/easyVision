{-# LANGUAGE CPP #-}

import EasyVision
import Data.List(transpose,minimumBy,foldl1',nub)
import Graphics.UI.GLUT hiding (Matrix,RGB,Size,minmax,histogram,Point,set)
import Debug.Trace
import Foreign
import ImagProc.Ipp.Core
import Text.Printf(printf)
import GHC.Float(float2Double)
import Control.Monad(when)
import Control.Parallel.Strategies
import Control.Concurrent
import ImagProc.InterestPoints
import Numeric.LinearAlgebra hiding ((.*))
import Vision(rot3,scaling,desp,unitary)
import ImagProc.ScaleSpace(Stage(..))
import GHC.Float(float2Double,double2Float)

inParallel x = parMap rnf id x

---------------------------------------------------------------

mkHessP = mkExtPyr k fun where
    k sigma = sigma^2/10
    fun = sqrt32f
        . thresholdVal32f 0 0 IppCmpLess
        . hessian

hsrespP sigma = sqrt32f
             . thresholdVal32f 0 0 IppCmpLess
             . hessian
             . gradients 
             . ((sigma^2/10) .*)
             . gaussS sigma

hsrespN sigma = sqrt32f
             . thresholdVal32f 0 0 IppCmpLess
             . ((-1).*)
             . hessian 
             . gradients 
             . ((sigma^2/10) .*)
             . gaussS sigma

laresp sigma = ((-1).*). (\g -> gxx g |+| gyy g)
             . gradients 
             -- . ((sigma/3) .*)
             . ((sigma^2/10) .*)
             . gaussS sigma

exper sigma = id
             . gaussS 2
             . abs32f
             . hessian
             . gradients 
             . ((sigma^2/10) .*)
             . gaussS sigma

hsrespPBox k = sqrt32f
             . thresholdVal32f 0 0 IppCmpLess
             . hessian
             . gradients
             . ((fromIntegral k^2/10) .*)
             . filterBox k k

main = do
    sz <- findSize

    (cam,ctrl) <- getCam 0 sz >>= withChannels >>= inThread >>= withPause

    prepare

    o <- createParameters [("sigma",realParam 1.0 0 3)
                          ,("steps",intParam 3 1 10)
                          ,("n",intParam 13  0 20)
                          ,("tot",intParam 200 1 500)
                          ,("h",realParam 0.3 0 2)
                          ,("rtest",intParam 10 0 100)
                          ,("dtest",intParam 0 0 100)
                          ,("what",intParam 0 0 2)
                          ,("mode",intParam 1 1 6)
                          ,("test",intParam 0 0 1)
                          ,("rot",realParam 0 (-180) 180)
                          ,("err",realParam 0.3 0 1)
                          ]

    w <- evWindow (False, Pixel 0 0, 0) "scale" sz Nothing  (mouse (kbdcam ctrl))
--    wd <- evWindow 0 "feature" sz Nothing (const (kbdcam ctrl))
--     wdebug <- evWindow () "debug" sz Nothing  (const (kbdcam ctrl))

    let wd = 1
    launchFreq 10 (worker o cam w wd)

-----------------------------------------------------------------

#define PAR(S) S <- getParam o "S"

worker o cam w wd = do

    PAR(rot)
    PAR(tot)   :: IO Int
    PAR(err)
    PAR(n)
    PAR(steps) :: IO Int
    PAR(sigma)
    PAR(rtest) :: IO Int
    PAR(dtest)  :: IO Int
    PAR(test)  :: IO Int
    PAR(mode)  :: IO Int
    PAR(h)
    PAR(what)  :: IO Int


    let sigmas = take (n+2) $ getSigmas sigma steps
--         boxes = take (n+2) [1,2,3,4,5,6,8,10,13,17,21,27,34,43,54]
--         sigmaboxes = map boxToSigma boxes

    orig <- cam
    let imr = if test == 1 then gaussS 1 $ blob rtest dtest
                           else (warp 0 (size (gray orig)) (rot3 (rot*pi/180)) $ float $ gray orig)

--         proc = case mode of
--             1 -> hsrespP
--             2 -> hsrespN
--             3 -> laresp
--             4 -> exper

--         hessianBox sigma = hsrespPBox (sigmaToBox sigma)

--         auxStd = sqrIntegral (toGray imr)
--         procStd sigma im = (2*1/255).* rectStdDev b b im where b = sigmaToBox sigma

--         responses = map (mkHessP imr) sigmas

--         (pts,pyr) = getInterestPoints 100 h (map pyrResp responses)

{-        (pts,pyr) = if mode < 5 then getInterestPoints (mkStage proc) sigmas 100 tot h imr
                                else case mode of
                                            5 -> getInterestPoints (mkStage hessianBox) sigmaboxes 100 tot h imr-}
--                                             6 -> getInterestPoints procStd sigmaboxes 100 tot h auxStd

{-        (gx,gy,_,_,_) = gradients $ (sc .*) $ gaussS 1 imr
        ga = abs32f gx |+| abs32f gy-}
--         feats = take tot $ concat $ reverse $ zipWith f (tail responses) pts
--             where f a bs = map (extractFeature a) bs

        descrip = --dummyFeat
                  (usurf 2 4)

        (feats1,pyr1) = multiscalePoints mkHessP descrip (take 5 sigmas) 100 h imr
        (feats2,pyr2) = multiscalePoints mkHessP descrip sigmas 100 h (decimate imr)

        --pyr = pyr1 --take 5 pyr1 ++ pyr2

        (feats',pyr) = multiscalePoints mkHessP descrip sigmas 100 h imr
        feats = concat $ reverse feats'

    (clicked,p,v) <- getW w
    when (clicked && not (null feats)) $ do
        let sel = minimumBy (compare `on` (dist p.ipRawPosition)) feats
        putW w (False,p, (ipDescriptor.ip) sel)

    let best = minimumBy (compare `on` (distv v.ipDescriptor.ip)) feats
        bestdist = distv (ipDescriptor.ip $ best) v
--     print $ map sigmaToBox sigmas
--     print $ map sigmaToBox sigmaboxes

--     let pyr = map (mkStage funbox imr) sigmaboxes

--     print $ map stSigma pyr
--     print $ map (sigmaToBox.stSigma) pyr
--     print $ map (round.stSigma) pyr

    
    --timing $ print $ length $ concat $ rawpts

  
    inWin w $ do
        when (what == 0) $ drawImage imr
        when (what == 2) $ drawImage (rgb orig)
        when (what == 1) $ drawImage (stResponse $ pyr!!n)
        lineWidth $= 1
        drawInterestPoints (size imr) (map ip feats)
        let w = width (size imr)
        mapM_ (boxFeat w) feats
        --text2D 20 20 (show $ map (size.stResponse) pyr)
--         let x = stResponse $ pyr!!n
--         text2D 20 20 $ show (minmax x)

--     inWin wdebug $ do
--      drawImage $ --maxLoc3 (stFiltMax $ pyr!!(n-1)) (stMaxLoc $ pyr!!n) (stFiltMax $ pyr!!(n+1))
--                      maxEvery (stFiltMax $ pyr!!(n-1)) (stFiltMax $ pyr!!(n+1))


        when (not (null feats) && bestdist < err) $ do
            boxFeat w best
            drawImage $ autoscale $  ipPatch best
            text2D 20 20 (printf "%.2f" bestdist)
            drawVector 50 100 $ 50*v
            drawVector 150 100 $ 50*(ipDescriptor.ip) best
            drawVector 50 200 $ 100*(unitary $ ipHistoOris best)


--     frame <- getW wd
--     when (frame==100) $ error "terminado"
--     putW wd (frame+1)

------------------------------------------------------------------

dist (Pixel a b) (Pixel r c) = (a-r)^2 + (b-c)^2

distv a b = pnorm PNorm2 (a-b)

sigmaToBox s = round $ (s * sqrt 12 - 1) / 2
boxToSigma b = (1 + 2 * fromIntegral b) / sqrt 12

expandROI x roi@(ROI r1 r2 c1 c2) = debug $ shrink (-d,-d) (debug roi)
    where d = ceiling ((x * s - s)/ 2)
          s =  fromIntegral $ max (c2-c1+1) (r2-r1+1)

-------------------------------------------------------------------






boxFeat w p = do
    let Point x y = ipPosition (ip p)
--     setColor 0 0.5 0
--     drawVector (c-18) (r+2*ipRawScale p) (50* (ipDescriptor.ip) p)
    setColor 1 1 1
    text2D (double2Float x) (double2Float y) (printf "%.1f" (0.5*fromIntegral w*ipScale (ip p)))
    --setColor 1 0 0
    --drawROI $ roiFromPixel (ipRawScale p) (ipRawPosition p)

-------------------------------------------------------------------

mouse _ st (MouseButton LeftButton) Down _ pos@(Position x y) = do
    (_,_,v) <- get st
    st $= (True, Pixel (fromIntegral y) (fromIntegral x), v)
mouse def _ a b c d = def a b c d

-------------------------------------

blob rad desp = unsafePerformIO $ do
    img <- image (Size 480 640)
    let vals = [[f r c | c <- [1..640]]| r <- [1..480]]
            where f r c = if (r-240-desp)^2 + (c-320-desp)^2 < rad^2 then 0.7 else 0.2
    setData32f img vals
    return img

-------------------------------------

copyTo sz roi im = unsafePerformIO $ do
    r <- image sz
    set 0 (theROI r) r
    let peq = resize (roiSize roi) im
    copy peq (theROI peq) r roi
    return r

autoscale im = f im
    where (mn,mx) = minmax im
          f = if mn == mx then scale32f8u 0 1 else scale32f8u mn mx

-------------------------------------

-- instance NFData Pixel where
--     rnf (Pixel r c) = rnf r
-- 
-- instance NFData Stage where
--     rnf s = rnf (stMaxLoc s)
-- 
-- instance NFData ImageFloat where
--     rnf (F x) = rwhnf (ptr x)

---------------------------------------

decimate orig = thresholdVal32f 0 0 IppCmpLess $ resize (Size h' w') orig
    where
    Size h w = size orig
    h' = h`div`2
    w' = w`div`2

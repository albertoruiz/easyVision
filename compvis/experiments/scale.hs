import EasyVision
import Data.List(transpose,minimumBy,foldl1',nub)
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point,set)
import Debug.Trace
import Foreign
import ImagProc.Ipp.Core
import Text.Printf(printf)
import GHC.Float(float2Double)
import Control.Monad(when)
import Control.Parallel.Strategies
import Control.Concurrent
import ImagProc.InterestPoints

inParallel x = parMap rnf id x

---------------------------------------------------------------

hsrespP sigma = sqrt32f
             . thresholdVal32f 0 0 IppCmpLess
             . hessian
             . secondOrder 
             . ((sigma^2/10) .*)
             . gaussS sigma


getSigmas sigma steps = [sigma*k^i | i <- [0..]] where k = 2**(1/fromIntegral steps)

---------------------------------------------------------------

hsrespN sigma = sqrt32f
             . thresholdVal32f 0 0 IppCmpLess
             . ((-1).*)
             . hessian 
             . secondOrder 
             . ((sigma^2/10) .*)
             . gaussS sigma

laresp sigma = ((-1).*). (\(_,_,gxx,gyy,_) -> gxx |+| gyy)
             . secondOrder 
             -- . ((sigma/3) .*)
             . ((sigma^2/10) .*)
             . gaussS sigma

exper sigma = id
             . gaussS 2
             . abs32f
             . hessian
             . secondOrder 
             . ((sigma^2/10) .*)
             . gaussS sigma

hsrespPBox k = sqrt32f
             . thresholdVal32f 0 0 IppCmpLess
             . hessian
             . secondOrder
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
                          ,("what",intParam 0 0 2)
                          ,("mode",intParam 1 1 6)
                          ,("test",intParam 0 0 1)
                          ]

    w <- evWindow (Pixel 200 200,[]) "scale" sz Nothing  (mouse (kbdcam ctrl))
    wd <- evWindow 0 "feature" (Size 200 200) Nothing (const (kbdcam ctrl))
--     wdebug <- evWindow () "debug" sz Nothing  (const (kbdcam ctrl))

    launchFreq 10 (worker o cam w wd)

-----------------------------------------------------------------

worker o cam w wd = do

    tot <- getParam o "tot" :: IO Int
    n <- getParam o "n" :: IO Int
    steps <- getParam o "steps" :: IO Int
    sigma <- getParam o "sigma" :: IO Float
    rtest <- getParam o "rtest" :: IO Int
    test <- getParam o "test" :: IO Int
    mode <- getParam o "mode" :: IO Int
    h <- getParam o "h" :: IO Float
    what <- getParam o "what" :: IO Int
    steps <- getParam o "steps" :: IO Int

    let sigmas = take (n+2) $ getSigmas sigma steps
        boxes = take (n+2) [1,2,3,4,5,6,8,10,13,17,21,27,34,43,54]
        sigmaboxes = map boxToSigma boxes

    orig <- cam
    let imr = if test == 1 then gaussS 2 $ blob rtest
                     else (float $ gray orig)

        proc = case mode of
            1 -> hsrespP
            2 -> hsrespN
            3 -> laresp
            4 -> exper

        hessianBox sigma = hsrespPBox (sigmaToBox sigma)

        auxStd = sqrIntegral (toGray imr)
        procStd sigma im = (2*1/255).* rectStdDev b b im where b = sigmaToBox sigma



        (pts,pyr) = if mode < 5 then getInterestPoints proc sigmas 100 tot h imr
                                else case mode of
                                            5 -> getInterestPoints hessianBox sigmaboxes 100 tot h imr
                                            6 -> getInterestPoints procStd sigmaboxes 100 tot h auxStd


--     print $ map sigmaToBox sigmas
--     print $ map sigmaToBox sigmaboxes

--     let pyr = map (mkStage funbox imr) sigmaboxes

--     print $ map stSigma pyr
--     print $ map (sigmaToBox.stSigma) pyr
--     print $ map (round.stSigma) pyr

    
    --timing $ print $ length $ concat $ rawpts

--     timing $
    inWin w $ do
        when (what == 0) $ drawImage imr
        when (what == 2) $ drawImage (rgb orig)
        when (what == 1) $ drawImage (stResponse $ pyr!!n)
        mapM_ box pts
        --text2D 20 20 (show $ map (size.stResponse) pyr)
--         let x = stResponse $ pyr!!n
--         text2D 20 20 $ show (minmax x)

--     inWin wdebug $ do
--      drawImage $ --maxLoc3 (stFiltMax $ pyr!!(n-1)) (stMaxLoc $ pyr!!n) (stFiltMax $ pyr!!(n+1))
--                      maxEvery (stFiltMax $ pyr!!(n-1)) (stFiltMax $ pyr!!(n+1))


    inWin wd $ when (not (null pts)) $ do
        (clicked,_) <- getW w
        let sel = minimumBy (compare `on` dist clicked) pts
            feat = float $ resize (Size 50 50) (modifyROI (const $ roiOf sel) $ gray orig)
        drawImage $ scale32f8u (-1) 1 $ sobelHoriz feat

    frame <- getW wd
    --when (frame==100) $ error "terminado"
    putW wd (frame+1)

------------------------------------------------------------------

box (Pixel r c, n) = do
    setColor 1 0 0
    renderPrimitive LineLoop $ do
        vertex (Pixel (r-n) (c-n))
        vertex (Pixel (r-n) (c+n))
        vertex (Pixel (r+n) (c+n))
        vertex (Pixel (r+n) (c-n))
    setColor 1 1 1
    text2D (fromIntegral c) (fromIntegral r) (show n)

roiOf (p, n) = roiFromPixel (3*n`div`2) p

roiFromPixel rad (Pixel r c) = ROI (r-rad) (r+rad) (c-rad)  (c+rad)

-- fixPts l s = map g l where g x = (x,round $ 1.7*s)

dist (Pixel a b) (Pixel r c, n) = (a-r)^2 + (b-c)^2

sigmaToBox s = round $ (s * sqrt 12 - 1) / 2
boxToSigma b = (1 + 2 * fromIntegral b) / sqrt 12

-------------------------------------------------------------------

mouse _ st (MouseButton LeftButton) Down _ pos@(Position x y) = do
    (_,l) <- get st
    st $= (Pixel (fromIntegral y) (fromIntegral x), l)
mouse _ st (Char 'y') Down _ _ = do
    (p,l) <- get st
    st $= (p, p:l)
mouse def _ a b c d = def a b c d

-------------------------------------

blob rad = unsafePerformIO $ do
    img <- image (Size 480 640)
    let vals = [[f r c | c <- [1..640]]| r <- [1..480]]
            where f r c = if (r-240)^2 + (c-320)^2 < rad^2 then 0.7 else 0.2
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

instance NFData Pixel where
    rnf (Pixel r c) = rnf r

instance NFData Stage where
    rnf s = rnf (stMaxLoc s)

instance NFData ImageFloat where
    rnf (F x) = rwhnf (ptr x)

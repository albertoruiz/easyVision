import EasyVision
import Data.List(transpose)
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point,set)
import Debug.Trace
import Foreign
import ImagProc.Ipp.Core
import Text.Printf(printf)
import GHC.Float(float2Double)
import Vision(scaling)

hsresp sigma = sqrt32f
             . thresholdVal32f 0 0 IppCmpLess 
             . hessian 
             . secondOrder 
             . ((sigma^2/10) .*)
             . gaussS sigma


main = do
    sz <- findSize

    (cam,ctrl) <- getCam 0 sz >>= withChannels >>= inThread >>= withPause

    prepare

    o <- createParameters [("sigma",realParam 1.0 0 3)
                          ,("steps",intParam 3 1 10)
                          ,("n",intParam 13  0 20)
                          ,("h",realParam 0.3 0 1)
                          ,("rtest",intParam 10 0 100)
                          ,("what",intParam 1 0 1)
                          ,("mode",intParam 1 1 3)
                          ,("test",intParam 0 0 1)
                          ]

    w <- evWindow () "scale" sz Nothing  (const (kbdcam ctrl))

    launchFreq 25 (worker o cam w)

-----------------------------------------------------------------

worker o cam w = do

    n <- getParam o "n" :: IO Int
    steps <- getParam o "steps" :: IO Int
    sigma <- getParam o "sigma" :: IO Float
    rtest <- getParam o "rtest" :: IO Int
    test <- getParam o "test" :: IO Int
    mode <- getParam o "mode" :: IO Int
    h <- getParam o "h" :: IO Float
    what <- getParam o "what" :: IO Int
    steps <- getParam o "steps" :: IO Int

    let sigmas = [sigma*k^n | n <- [0..]] where k = 2**(1/fromIntegral steps)

    orig <- cam
    let imr = if test == 1 then blob rtest
                     else (float $ gray orig)

        proc = case mode of
            1 -> hsresp
            2 -> laresp
            3 -> mixed

        pyr = map (flip proc imr) sigmas

        rawpts = siftPoints 100 h pyr sigmas
        pts = zipWith (fixPts steps sigma) rawpts [1..]

    inWin w $ do
        drawImage $ if what == 0 then (pyr!!n) else imr
        mapM_ box (concat (take n pts))
------------------------------------------------------------------

siftPoints nmax h imgs sigmas = zipWith3 (localMaxScale3 nmax h) l1 l2 l3
    where l1 = zipWith g imgs sigmas
          l2 = zipWith f (tail imgs) (tail sigmas)
          l3 = zipWith g (tail $ tail imgs) (tail $ tail sigmas)
          f im s = localMax (round (2*s)) im
          g im s = filterMax32f (round (2*s)) im

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

fixPts steps sigma l n = map g l where g x = (x,round $ 1.5*sigma*k^n) where k = 2**(1/fromIntegral steps)

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

laresp sigma = 
              (\(_,_,gxx,gyy,_) -> gxx |+| gyy)
             . secondOrder 
             . ((sigma^2/10) .*)
             . gaussS sigma


mixed sigma = sqrt32f
             . thresholdVal32f 0 0 IppCmpLess
             . (\(_,_,gxx,gyy,gxy) -> (gxx|*|gyy |-| gxy |*|gxy) |*| (gxx |+| gyy) |*| (gxx |+| gyy))
             . secondOrder 
             . ((sigma^2/10) .*)
             . gaussS sigma

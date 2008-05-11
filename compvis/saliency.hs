--experiment...

import EasyVision
--import ImagProc.Ipp.Core(intersection)
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point,set)
--import GHC.Float(float2Double)
--import Text.Printf
import Control.Monad
--import Data.List(transpose)


------------------------------------------------------------

main = do

    prepare

    sz <- findSize

    (cam, ctrl)  <- getCam 0 sz  >>= withChannels >>= withPause

    w <- evWindow () "image" sz Nothing  (const (kbdcam ctrl))

    o <- createParameters [("rad"  ,intParam 25 1 30),
                           ("sigma",realParam 1.6 0.1 10),
                           ("sigma2",realParam 5 0.1 20),
                           ("thres",realParam 0.3 0 1),
                           ("n",intParam 4 0 20),
                           ("what",intParam 0 0 1)]


    launch $ do
        inWin w $ do
            orig <- cam
            th <- getParam o "thres"
            rad <- getParam o "rad"
            sigma <- getParam o "sigma"
            sigma2 <- getParam o "sigma2"
            --nmax <- getParam o "n"
            what <- getParam o "what" :: IO Int
            let img = (gaussS sigma2 . sqrt32f . abs32f . hessian . secondOrder. gaussS sigma. float . gray) orig
            let pts = getPoints32f 100 . thresholdVal32f th 0 IppCmpLess . localMax (2*rad+1) $ img
            if what == 0
                then drawImage (gray $ orig)
                else drawImage (img)
            pointCoordinates sz
            pointSize $= 5
            setColor 1 0 0
            when (not (null pts)) $ do
                renderPrimitive Points (mapM_ vertex (pixelsToPoints sz pts))


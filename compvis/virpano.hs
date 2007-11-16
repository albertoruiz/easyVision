-- new interface

module Main where

import EasyVision
import System.Environment(getArgs)
import qualified Data.Map as Map
import Graphics.UI.GLUT hiding (Matrix, Size, Point)
import Vision
import Control.Monad(when)
import Numeric.LinearAlgebra
import Data.IORef
---------------------------------------------------------------
import NewParam
----------------------------------------------------------------

szDst = Size 320 640

horizPano cam1 cam2 = do
    wr1 <- warper "warper1"
    wr2 <- warper "warper2"
    return $ do
        orig1 <- cam1
        floor <- image szDst
        set32f 0 (theROI floor) floor
        (rh1,_) <- getW wr1
        h1 <- rh1
        warpOn h1 floor orig1
        orig2 <- cam2
        (rh2,_) <- getW wr2
        h2 <- rh2
        warpOn h2 floor orig2
        return floor

warperHomog pan tilt rho foc sca =
        scaling sca
        <> kgen foc
        <> rot1 tilt
        <> rot2 pan 
        <> rot3 rho 
        <> kgen (1/foc)

warper name = do
    (_,param) <- createParameters'  [ ("pan",  realParam (0) (-40) (40))
                                     ,("tilt", realParam (0) (-30) (30))
                                     ,("rho",  realParam  0 (-60) (60))
                                     ,("foc",  listParam 2.8 [0.5, 0.7, 1, 2, 2.8, 5, 5.5, 9,10])
                                     ,("sca",  listParam 0.5 [1.1**k|k<-[-20..20]])]
    let sz = Size 300 400
        h = do
            pan   <- getParam param "pan"
            tilt  <- getParam param "tilt"
            rho   <- getParam param "rho"
            foc   <- getParam param "foc"
            sca   <- getParam param "sca"
            let t = warperHomog (pan*degree) (tilt*degree) (rho*degree) foc sca
            return t
        f img = do
            t <- h
            warp sz t img

    let drw w img = do
        inWin w $ do
            windowStatus $= Shown
            f img >>= drawImage

    w <- evWindow undefined name sz Nothing (const kbdQuit)
    windowStatus $= Hidden
    putW w (h,drw w)
    return w


main = do
    args <- getArgs

    let opts = Map.fromList $ zip args (tail args)
        sz   = findSize args

    prepare'

    (cam1,ctrl1) <- mplayer (args!!0) sz >>= withPause
    (cam2,ctrl2) <- mplayer (args!!1) sz >>= withPause
    (cam3,ctrl3) <- mplayer (args!!2) sz >>= withPause

    let c1 = cam1 >>= yuvToGray >>= scale8u32f 0 1
        c2 = cam2 >>= yuvToGray >>= scale8u32f 0 1
        c3 = cam3 >>= yuvToGray >>= scale8u32f 0 1

    wDest  <- evWindow () "pano" szDst Nothing (mouse (kbdQuit))

    cam12 <- horizPano c1 c2
    cam <- horizPano cam12 c3

    launch' (worker cam wDest)

-----------------------------------------------------------------

worker cam wDest = do
    inWin wDest $
        cam >>= drawImage

---------------------------------------------------------

mouse _ st (MouseButton LeftButton) Down _ _ = do
    st $= ()

mouse def _ a b c d = def a b c d

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

{-
h10 = (3><3) [0.5,0,0.5,
              0  ,0.5,0,
              0  , 0, 1] 

h20 = (3><3) [0.5,0,-0.5,
              0  ,0.5,0,
              0  , 0, 1]
-}

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
        inWin w $ f img >>= drawImage

    w <- evWindow undefined name sz Nothing (const kbdQuit)
    putW w (h,drw w)
    return w




main = do
    args <- getArgs

    let opts = Map.fromList $ zip args (tail args)
        sz   = findSize args

    prepare'

    (cam1,ctrl1) <- mplayer (args!!0) sz >>= withPause
    (cam2,ctrl2) <- mplayer (args!!1) sz >>= withPause

    --wimg1 <- evWindow () "image1" sz Nothing (const $ kbdcam ctrl1)
    --wimg2 <- evWindow () "image2" sz Nothing (const $ kbdcam ctrl2)
    wDest  <- evWindow () "pano" szDst Nothing (mouse (kbdQuit))

    wr1 <- warper "warper1"
    wr2 <- warper "warper2"

    launch' (worker cam1 cam2 wDest wr1 wr2)

-----------------------------------------------------------------

inWindow = undefined


worker cam1 cam2 wDest wr1 wr2 = do

    orig1 <- cam1 >>= yuvToGray >>= scale8u32f 0 1 
    orig2 <- cam2 >>= yuvToGray >>= scale8u32f 0 1 

    (rh1,w1) <- getW wr1
    w1 orig1

    (rh2,w2) <- getW wr2
    w2 orig2

    inWin wDest $ do
        floor <- image szDst
        set32f 0 (theROI floor) floor
        h1 <- rh1
        warpOn h1 floor orig1
        h2 <- rh2
        warpOn h2 floor orig2
        drawImage floor


---------------------------------------------------------

a4aux = [[-1,-r],[1,-r],[1,r],[-1,r]]
    where r = 1/sqrt 2

a4 = [[   0,    0]
     ,[   0, 2.97]
     ,[2.10, 2.97]
     ,[2.10,    0]]

vector l = fromList l :: Vector Double

diagl = diag .vector

pl (Point x y) = [x,y]

alter pts = map (rotateList pts) [0 .. 3]

rotateList list n = take (length list) $ drop n $ cycle list

drawSeg s = do
    vertex $ (extreme1 s)
    vertex $ (extreme2 s)

isA4 mbf tol pts = ao < tol && cy < 0
    where mbomega = fmap omegaGen mbf
          ao = autoOrthogonality mbomega h
          h = estimateHomography (map pl pts) a4
          Just p = poseFromHomogZ0 mbf h
          (_,cy,_) = cameraCenter p

omegaGen f = kgen (recip (f*f))

text2D x y s = do
    rasterPos (Vertex2 x (y::GLfloat))
    renderString Helvetica12 s

mouse _ st (MouseButton LeftButton) Down _ _ = do
    st $= ()

mouse def _ a b c d = def a b c d

warperHomog pan tilt rho foc sca =
        scaling sca
        <> kgen foc
        <> rot1 tilt
        <> rot2 pan 
        <> rot3 rho 
        <> kgen (1/foc)
 
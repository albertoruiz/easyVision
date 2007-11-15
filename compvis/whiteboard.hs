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

----------------------------------------------------------------

szA4 = (Size (32*10) (32*14))

main = do
    args <- getArgs

    let opts = Map.fromList $ zip args (tail args)
        sz   = findSize args

    prepare'

    (cam,ctrl) <- mplayer (args!!0) sz >>= withPause

    wimg <- evWindow () "image" sz Nothing (const $ kbdcam ctrl)
    wa4  <- evWindow (ident 3) "a4" szA4 Nothing (mouse (kbdcam ctrl))

    launch' (worker cam wimg wa4)

-----------------------------------------------------------------

inWindow = undefined


worker cam wImage wA4 = do

    orig <- cam >>= yuvToGray

    let minlen = 0.15
        radius = 4
        width = 1.5
        median = 5
        high = 40
        low = 20
        pp = True
        orthotol = 0.5
        maxdis = 0.06
        mbf = Nothing
        segs = filter ((>minlen).segmentLength) $ segments radius width median high low pp orig
        polis = segmentsToPolylines maxdis segs
        closed4 = [p | Closed p <- polis, length p == 4]
        a4s = filter (isA4 mbf orthotol) (concatMap alter closed4)

    inWin wImage $ do
        drawImage orig

        pointCoordinates (size orig)

        setColor 0 0 1
        lineWidth $= 1
        renderPrimitive Lines $ mapM_ drawSeg segs

        setColor 1 0 0
        lineWidth $= 3
        mapM_ (renderPrimitive LineLoop . (mapM_ vertex)) closed4

        setColor 0 1 0
        pointSize $= 5
        mapM_ (renderPrimitive Points . (mapM_ vertex)) closed4

    inWin wA4 $ do
        when (length a4s >0) $ do
            let pts = head a4s
                h = estimateHomography a4aux (map pl pts)
            putW wA4 h

        h <- getW wA4
        imf <- scale8u32f 0 1 orig
        floor <- warp szA4 h imf
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
    st $= ident 3

mouse def _ a b c d = def a b c d

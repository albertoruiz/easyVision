-- very basic "augmented reality" on an A4 sheet
-- examples:
-- ./augmented tv://

module Main where

import EasyVision
import ImagProc.C.Segments
import System.Environment(getArgs)
import qualified Data.Map as Map
import Graphics.UI.GLUT hiding (Matrix, Size, Point)
import Vision
import Control.Monad(when)
import Numeric.LinearAlgebra
import Debug.Trace
import Vision.Autofrontal
import Util.Options

debug x = trace (show x) x

main = do
    sz <- findSize

    (cam,ctrl) <- getCam 0 sz >>= withPause

    prepare

    o <- createParameters     [("radius",intParam 4 0 10),
                               ("width",realParam 1.5 0 5),
                               ("median",intParam 5 3 5),
                               ("high",intParam 40 0 255),
                               ("low",intParam 20 0 255),
                               ("postproc",intParam 1 0 1),
                               ("minlength",realParam 0.15 0 1),
                               ("maxdis",realParam 0.06 0 0.1),
                               ("orthotol",realParam 0.25 0.01 0.5)]

    w <- evWindow () "image" sz Nothing (const $ kbdcam ctrl)
    depthFunc $= Just Less

    mbf <- maybeOption "--focal"

    launch (worker cam o mbf w)

-----------------------------------------------------------------


worker cam op mbf w = do

    radius <- getParam op "radius"
    width  <- getParam op "width"
    median <- getParam op "median"
    high   <- fromIntegral `fmap` (getParam op "high" :: IO Int)
    low    <- fromIntegral `fmap` (getParam op "low" :: IO Int)
    postp  <- getParam op "postproc" :: IO Int
    let pp = if postp == 0 then False else True
    minlen <- getParam op "minlength"
    maxdis <- getParam op "maxdis"
    orthotol  <- getParam op "orthotol"

    orig <- cam >>= return . yuvToGray
    let segs = filter ((>minlen).segmentLength) $ segments radius width median high low pp orig
        polis = segmentsToPolylines maxdis segs
        closed4 = [p | Closed p <- polis, length p == 4]
        a4s = filter (isA4 mbf orthotol) (concatMap alter closed4)

    inWin w $ do
        clear [DepthBuffer]
        drawImage orig
        clear [DepthBuffer]
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

        when (length a4s >0) $ do
            let pts = head a4s
                camera = cameraFromPlane 1E-3 500 mbf (map pl pts) a4

            case camera of
                Nothing -> return ()
                Just (p,path) -> do
                    clear [DepthBuffer]
                    --dispR 5 (dropRows (rows path - 5) path)

                    cameraView p (4/3) 0.1 100

                    setColor 0 0 1
                    lineWidth $= 1
                    renderPrimitive LineLoop (mapM_ vertex a4)

                    houseModel

    return ()

---------------------------------------------------------

a4 = [[   0,    0]
     ,[   0, 2.97]
     ,[2.10, 2.97]
     ,[2.10,    0]]

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



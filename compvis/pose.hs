-- automatic pose from A4 using segments
-- examples:
-- ./pose frontal.dv --size 12
-- ./pose tv:// --focal 2.45

module Main where

import EasyVision
import System.Environment(getArgs)
import qualified Data.Map as Map
import Graphics.UI.GLUT hiding (Matrix, Size, Point)
import Vision
import Control.Monad(when)
import LinearAlgebra

main = do
    args <- getArgs

    let opts = Map.fromList $ zip args (tail args)

    let sz = if Map.member "--size" opts
                 then mpSize $ read $ Map.findWithDefault "20" "--size" opts
                 else Size (read $ Map.findWithDefault "480" "--rows" opts)
                           (read $ Map.findWithDefault "640" "--cols" opts)

    (cam,ctrl) <- mplayer (args!!0) sz >>= withPause

    (tb,kc,mc) <- newTrackball

    app <- prepare ()

    o <- createParameters app [("radius",intParam 4 0 10),
                               ("width",realParam 1.5 0 5),
                               ("median",intParam 5 3 5),
                               ("high",intParam 40 0 255),
                               ("low",intParam 20 0 255),
                               ("postproc",intParam 1 0 1),
                               ("minlength",realParam 0.15 0 1),
                               ("maxdis",realParam 0.06 0 0.1),
                               ("scale",realParam 0.2 0.01 1),
                               ("orthotol",realParam 0.25 0.01 0.5)]

    addWindow "image" sz Nothing (const $ kbdcam ctrl) app

    addWindow "3D view" (Size 400 400) Nothing undefined app
    keyboardMouseCallback $= Just (kc (kbdcam ctrl))
    motionCallback $= Just mc
    depthFunc $= Just Less
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureFunction $= Replace

    let mbf = read `fmap` Map.lookup "--focal" opts

    launch app (worker cam o tb mbf)

-----------------------------------------------------------------


worker cam op trackball mbf inWindow _ = do

    radius <- getParam op "radius"
    width  <- getParam op "width"
    median <- getParam op "median"
    high   <- fromIntegral `fmap` (getParam op "high" :: IO Int)
    low    <- fromIntegral `fmap` (getParam op "low" :: IO Int)
    postp  <- getParam op "postproc" :: IO Int
    let pp = if postp == 0 then False else True
    minlen <- getParam op "minlength"
    maxdis <- getParam op "maxdis"
    scale  <- getParam op "scale"
    orthotol  <- getParam op "orthotol"

    orig <- cam >>= yuvToGray
    let segs = filter ((>minlen).segmentLength) $ segments radius width median high low pp orig
        polis = segmentsToPolylines maxdis segs
        closed4 = [p | Closed p <- polis, length p == 4]

    inWindow "image" $ do
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

    let a4s = filter (isA4 mbf orthotol) (concatMap alter closed4)

    inWindow "3D view" $ do
        clear [ColorBuffer, DepthBuffer]
        trackball

        setColor 0 0 1
        lineWidth $= 2
        renderPrimitive LineLoop (mapM_ vertex a4)

        --when (length closed4 >0) $ do
        --    let pts = head closed4
        --    mapM_ (posib' mbf orthotol orig) (alter pts)

        when (length a4s >0) $ do
            let pts = head a4s
                h = estimateHomography a4 (map pl pts)
                Size _ sz = size orig
            imf <- scale8u32f 0 1 orig
            floor <- warp (Size 256 256) (scaling scale <> h) imf
            drawTexture floor $ map (++[-0.01]) $ ht (scaling (1/scale)) [[1,1],[-1,1],[-1,-1],[1,-1]]

            imt <- extractSquare 128 imf
            let Just (cam,path) = cameraFromPlane 1E-3 500 mbf (map pl pts) a4
            --let Just cam = cameraFromHomogZ0 mbf (inv h)
            drawCamera 1 cam (Just imt)

            --pointCoordinates (Size 400 400)
            --setColor 1 1 1
            --text2D 0.95 (-0.95) (show $ focalFromHomogZ0 $ inv h)

    return ()

---------------------------------------------------------

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

-- experiments on multiview calibration from stabilized pose

import EasyVision hiding ((.*))
import Graphics.UI.GLUT hiding (Matrix, Size, Point,set)
import Vision
import Numeric.LinearAlgebra
import Control.Monad(when)

main = do
    prepare

    sz <- findSize
    n <- numCams

    mbf <- maybeOption "--focal"

    let ref = map (map (/3)) asym'
        vc c = c >>= withChannels >>= findPolygons mbf ref
    cams <- mapM (vc . flip getCam sz) [0..n-1]


    let initState = replicate n cameraAtOrigin


    w <- evWindow 0 "multipose" sz Nothing (mouse $ kbdQuit)
    w3D <- evWindow3D () "3D view" 400 (const $ kbdQuit)
    w3DSt <- evWindow3D initState "Camera Reference" 400 (const $ kbdQuit)

    launch $ do
        (imgs,det) <- unzip `fmap` sequence cams

        let rects = map (map fst) det
            ps    = map (map (syntheticCamera.snd)) det

        inWin w $ do -- monitorization window
            c' <- getW w
            let c = c' `mod` n
                img = imgs!!c
                rect = rects!!c

            drawImage (gray img)
            pointCoordinates (size (gray img))

            setColor 1 0 0
            lineWidth $= 3
            mapM_ (renderPrimitive LineLoop . (mapM_ vertex)) rect
            text2D 0.9 0.6 (show (length rect))

        inWin w3D $ do -- reference world
            setColor 0 0 1
            lineWidth $= 2
            renderPrimitive LineLoop (mapM_ vertex ref)

            lineWidth $= 1
            mapM_ (\c -> drawCamera 1 c Nothing) (concat ps)


        st' <- getW w3DSt
        let st = update st' ps
        putW w3DSt st

        inWin w3DSt $ do -- camera reference
            setColor 1 0 0
            lineWidth $= 1
            mapM_ (\c -> drawCamera 1 c Nothing) st



update st' ps = st where
    gcs = map (map (fst.toCameraSystem)) ps
    obs = map (map (betterRep.(<>( (gcs!!0!!0))))) ps
    st = if (length (concat ps)==length st')
        then map asMatrix $ zipWith (weighted 0.95) (map betterRep st') (concat obs)
        else st'


betterRep = factorizeCamera
asMatrix (k,r,c) = k <> (r <|> -r <> c)

weighted alpha (k1,r1,c1) (k2,r2,c2) = (alpha.*k1+(1-alpha).*k2,
                                        fix $ alpha.*r1+(1-alpha).*r2, -- !!!
                                        alpha.*c1+(1-alpha).*c2)
    where fix m = u<>trans v
              where (u,_,v) = svd m

------------------------------------------------

asym = [ [ 0, 0]
       , [ 0, 9.7]
       , [ 5, 9.7]
       , [ 5, 14.9]
       , [ 10.5, 14.9]
       , [ 10.5, 0] ]

asym' = [ [ 0, 0]
       , [ 0, 9.7]
       , [ 5, 14.9]
       , [ 10.5, 14.9]
       , [ 10.5, 0] ]

a4 = [[   0,            0]
     ,[   0, (2.10*ratio)]
     ,[2.10, (2.10*ratio)]
     ,[2.10,           0]] where ratio = sqrt 2


mouse _ st (MouseButton WheelUp) Down _ _ = do
    st $~ (+1)
mouse _ st (MouseButton WheelDown) Down _ _ = do
    st $~ (subtract 1)
mouse def _ a b c d = def a b c d

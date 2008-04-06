import EasyVision
import Vision
import Graphics.UI.GLUT hiding (Matrix, Size, Point,set)
import Text.Printf
import Numeric.LinearAlgebra

disp m = putStrLn . format " " (printf "%5.2f") $ m

asym = map (map (*0.54))
       [ [ 0, 0]
       , [ 0, 2]
       , [ 1, 2]
       , [ 2, 1]
       , [ 2, 0] ]

main = do
    prepare
    sz <- findSize
    mbf <- maybeOption "--focal"
    let ref = asym

    cam <- getCam 0 sz >>= withChannels >>= findPolygons mbf ref >>= poseTracker mbf ref

    w <- evWindow () "img" sz Nothing (const $ kbdQuit)
    w3D <- evWindow3D () "UKF 3D pose tracker" 500 (const $ kbdQuit)

    launch $ do
        (img,pose,(st,cov),obs) <- cam

        inWin w $ do
            drawImage (gray img)

            pointCoordinates (size (gray img))

            setColor 1 0 0
            lineWidth $= 3
            renderPrimitive LineLoop $ mapM_ vertex (ht (syntheticCamera pose) (map (++[0]) ref))

            --disp $ fromRows [st, sqrt $ takeDiag cov]
            --disp $ cov

            case obs of
                Nothing -> return ()
                Just (fig,p) -> do
                    setColor 0 0.5 0
                    lineWidth $= 3
                    renderPrimitive LineLoop $ mapM_ vertex fig

        inWin w3D $ do

            let scale = 0.5
                h = inv (f $ syntheticCamera pose) where f =  fromColumns . g. toColumns where g [a,b,c,d] = [a,b,d] 
                floor = warp 0 (Size 256 256) (scaling scale <> h) (float $ gray img)
            drawTexture floor $ map (++[-0.01]) $ ht (scaling (1/scale)) [[1,1],[-1,1],[-1,-1],[1,-1]]

            setColor 1 0 0
            lineWidth $= 2
            renderPrimitive LineLoop (mapM_ vertex ref)

            lineWidth $= 1
            setColor 0 0 1
            drawCamera 0.4 (syntheticCamera pose) (Just (extractSquare 128 (float $ gray img)))

            pixelCoordinates (Size 500 500)
            setColor 0 0.5 0.5
            text2D 20 20 (shPose pose)


shPose (CamPar f p t r (cx,cy,cz)) = printf "f=%.2f  pan=%.1f  tilt=%.1f  roll=%.1f (%.1f, %.1f, %.1f)" 
                                            f (p*degree) (t*degree) (r*degree) (cx*cm) (cy*cm) (cz*cm)
    where degree = 180/pi
          cm = 10

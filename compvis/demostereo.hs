-- tracking a blue region

import EasyVision hiding (State)
import Control.Monad((>=>),when)
import Graphics.UI.GLUT hiding (Point,Matrix,matrix,Size,triangulate)
import Data.List
import Numeric.LinearAlgebra
import Vision
import Text.Printf(printf)

tracker n1 n2 = withChannels >=> regionDetector n1 >=> regionTracker n2

disp n = putStrLn . format "  " (printf $ "%."++ show n++"f")
mean l = sum l / genericLength l

norm (vx,vy) = sqrt $ vx * vx + vy*vy

center cam = toList $ inHomog $ nullVector cam

stc0 = kgen 2 <>
        (3><4) [ 1, 0, 0, -3,
                 0, 1, 0,  0,
                 0, 0, 1,  0 ]

stc1 = kgen 2 <>
        (3><4) [ 1, 0, 0, 3,
                 0, 1, 0, 0,
                 0, 0, 1, 0 ]

main = do
    sz <- findSize
    prepare

    cam0 <- getCam 0 sz >>= tracker ""           "Kalman 1"
    cam1 <- getCam 1 sz >>= tracker "Detector 2" ""

    (trackball,kc,mc) <- newTrackball
    w3D <- evWindow (True,(stc0,[],stc1,[])) "3D view" (Size 640 640) Nothing (mouse $ kc kbdQuit)
    motionCallback $= Just mc
    depthFunc $= Just Less
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureFunction $= Replace

    launch $ inWin w3D $ do
       clear [ColorBuffer, DepthBuffer]
       trackball
       (orig0,(p1,v1)) <- cam0
       (orig1,(p2,v2)) <- cam1

       (wasMoving,(c0,pts1,c1,pts2)) <- getW w3D

       setColor 1 1 1
       imt0 <- extractSquare 128 (float $ gray $ orig0)
       
       drawCamera 0.2 c0 (Just imt0)

       imt1 <- extractSquare 128 (float $ gray $ orig1)
       
       drawCamera 0.2 c1 (Just imt1)

       let Point x1 y1 = p1
           Point x2 y2 = p2
           [[x,y,z]] = triangulate [(c0, [[x1,y1]]), (c1, [[x2,y2]])]

       setColor 0.5 0.5 1
       renderPrimitive LineStrip $ mapM_ vertex [center c0,[x,y,z],center c1]
       setColor 0 0 1
       pointSize $= 5
       renderPrimitive Points $ mapM_ vertex [[x,y,z]]


       

       let stopped = norm v1 < 1/640 && norm v2 < 1/640
           moveAgain = norm v1 > 5/640 || norm v2 > 5/640

       when (wasMoving && stopped) $ do
            print (p1,p2)
            putW w3D (False,(c0,([x1,y1]:pts1),c1,([x2,y2]:pts2)))

       when moveAgain $ do
            putW w3D (True,(c0,pts1,c1,pts2))

       let pts3d = triangulate [(c0, pts1), (c1, pts2)]
       setColor 0.5 0 0
       pointSize $= 3
       renderPrimitive LineStrip $ mapM_ vertex pts3d
       setColor 1 0 0
       renderPrimitive Points $ mapM_ vertex pts3d

       when (length pts1>7) $ do
            let f = estimateFundamental pts2 pts1
            putStrLn "OK Fundamental matrix:"
            disp 8 (normat f)
            putStr "mean epipolar distance: "
            print $ mean $ epipolarQuality f pts2 pts1
            let (e,df,err) = estimateEssential 0.1 f
            putStr "Estimated common f using equalization: "
            print df
            putStr "with quality: "
            print err
            let m = kgen df <> cameraAtOrigin
            let ms = map (kgen df <>) (camerasFromEssential e)
            let m' = selectCamera (head pts1) (head pts2) m ms
            when (err < 3E-2) $ do
                (wasMoving,(_,pts1,_,pts2)) <- getW w3D
                putW w3D (wasMoving,(m,pts1,m',pts2))


mouse _ st (Char 'x') Down _ _ = do
    (wasMoving,(c0,pts1,c1,pts2)) <- get st
    st $= (False, (c0,[], c1,[]))

mouse _ st (Char 'd') Down _ _ = do
    (wasMoving,(c0,pts1,c1,pts2)) <- get st
    when (not (null pts1)) $ do
        st $= (False, (c0,tail pts1, c1,tail pts2))

mouse def _ a b c d = def a b c d

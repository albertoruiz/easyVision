-- triangulation from several views
-- (calibrated with ../pose/multipose)

import EasyVision
import Control.Monad((>=>),when)
import Graphics.UI.GLUT hiding (Point,Matrix,matrix,Size,triangulate)
import Data.List
import Numeric.LinearAlgebra
import Vision
import Text.Printf(printf)
import Util.Options

tracker n1 n2 = withChannels >=> regionDetector n1 >=> regionTracker n2

disp n = putStrLn . format "  " (printf $ "%."++ show n++"f")
mean l = sum l / genericLength l

norm (vx,vy) = sqrt $ vx * vx + vy*vy

center cam = toList $ inHomog $ nullVector cam

withChannels = return . fmap channels


main = do
    prepare

    mbf <- getRawOption "--calib"
    let file = case mbf of
                Nothing -> "cameras.txt"
                Just file -> file
    campars <- read `fmap` readFile file

    sz <- findSize
    n <- numCams

    let vc c = c >>= tracker "" "Tracker"
    cams <- mapM (vc . flip getCam sz) [0..n-1]

    w3D <- evWindow3D (False,[]) "3D sensor" 640 (mouse kbdQuit)
    clearColor $= Color4 1 1 1 1
    lineSmooth $= Enabled

    launch $ inWin w3D $ do

       (imgs,points) <- unzip `fmap` sequence cams

       (wanted,l) <- getW w3D

       setColor 0.5 0 0
       pointSize $= 3
       renderPrimitive LineStrip $ mapM_ vertex l
       setColor 1 0 0
       renderPrimitive Points $ mapM_ vertex l

       setColor 0 0 0
       lineWidth $= 1.5

       let f c im = drawCamera 0.4 c (Just $ extractSquare 256 $ float $ gray im)
       sequence $ zipWith f campars imgs

       let multi = [ (c,[[x,y]]) | (c,(l,(Point x y),_)) <- zip campars points, abs x < 0.95 && abs y < 0.65, l < 10]

       when (length multi > 1) $ do
            let [[x,y,z]] = triangulate multi

            setColor 0.5 0.5 1
            mapM_ (\c->renderPrimitive LineStrip $ mapM_ vertex [[x,y,z],c]) (map (center.fst) multi)
            setColor 0 0 1
            pointSize $= 5
            renderPrimitive Points $ mapM_ vertex [[x,y,z]]

            when (not . null $ l) $ do
                setColor 0.3 0.3 0.3
                renderPrimitive Lines $ mapM_ vertex [[x,y,z],(head l)]

            when wanted $ do
                putW w3D (False,[x,y,z]:l)

            pixelCoordinates (Size 640 640)
            setColor 0 0 0
            text2D 20 20 (printf "x=%.1f y=%.1f z=%.1f" (10*x) (10*y) (10*z))
            when (not . null $ l) $ do
                let [a,b,c] = head l
                text2D 20 60 (printf "dist = %.1f cm" (10 * sqrt ((x-a)^2 + (y-b)^2 + (z-c)^2)))


mouse _ st (Char ' ') Down _ _ = do
    (_,l) <- getW st
    putW st (True, l)

mouse _ st (Char 'x') Down _ _ = do
    (_,l) <- getW st
    putW st (False, if null l then l else tail l)

mouse def _ a b c d = def a b c d

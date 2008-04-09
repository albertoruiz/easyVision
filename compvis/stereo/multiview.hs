-- triangulation from several views
-- (calibrated with ../pose/multipose)

import EasyVision hiding (State)
import Control.Monad((>=>),when)
import Graphics.UI.GLUT hiding (Point,Matrix,matrix,Size,triangulate)
import Data.List
import Numeric.LinearAlgebra
import Vision
import Text.Printf(printf)



instance (Element a, Read a) => Read (Matrix a) where
    readsPrec _ s = [(reshape 4 . fromList . read $ thing, rest)]
        where (a,b) = break (==']') s
              rest = tail b
              thing = drop 7 a ++ "]"


tracker n1 n2 = withChannels >=> regionDetector n1 >=> regionTracker n2

disp n = putStrLn . format "  " (printf $ "%."++ show n++"f")
mean l = sum l / genericLength l

norm (vx,vy) = sqrt $ vx * vx + vy*vy

center cam = toList $ inHomog $ nullVector cam

main = do
    prepare

    file <- getOption "--calib" "cameras.txt"
    campars <- read `fmap` readFile file

    sz <- findSize
    let n = 2-- length cams

    let vc c = c >>= tracker "" "Tracker"
    cams <- mapM (vc . flip getCam sz) [0..n-1]

    w3D <- evWindow3D (False,[]) "3D sensor" 640 (mouse kbdQuit)

    launch $ inWin w3D $ do

       (imgs,points) <- unzip `fmap` sequence cams

       (wanted,l) <- getW w3D

       setColor 0.5 0 0
       pointSize $= 3
       renderPrimitive LineStrip $ mapM_ vertex l
       setColor 1 0 0
       renderPrimitive Points $ mapM_ vertex l

       setColor 1 1 1

       let f c im = drawCamera 0.4 c (Just $ float $ gray im)
       sequence $ zipWith f campars imgs

       let ok [[x,y]] = abs x < 0.95 && abs y < 0.65
           prep (Point x y) = [[x,y]]
           multi = filter (ok.snd) $ zip campars (map (prep.fst) points)

       when (length multi > 1) $ do
            let [[x,y,z]] = triangulate multi

            setColor 0.5 0.5 1
            mapM_ (\c->renderPrimitive LineStrip $ mapM_ vertex [[x,y,z],c]) (map center campars)
            setColor 0 0 1
            pointSize $= 5
            renderPrimitive Points $ mapM_ vertex [[x,y,z]]

            when (not . null $ l) $ do
                setColor 0.3 0.3 0.3
                renderPrimitive Lines $ mapM_ vertex [[x,y,z],(head l)]

            when wanted $ do
                putW w3D (False,[x,y,z]:l)

            pixelCoordinates (Size 640 640)
            setColor 1 1 1
            text2D 20 20 (printf "x=%.2f y=%.2f z=%.2f" x y z)


mouse _ st (Char ' ') Down _ _ = do
    (_,l) <- get st
    st $= (True, l)

mouse _ st (Char 'x') Down _ _ = do
    (_,l) <- get st
    st $= (False, if null l then l else tail l)

mouse def _ a b c d = def a b c d

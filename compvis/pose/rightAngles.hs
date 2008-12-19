-- zoom window: click to center, wheel to zoom

import EasyVision
import Graphics.UI.GLUT hiding (Size, Point)
import System(getArgs)
import Control.Monad(when)
import Numeric.LinearAlgebra
import Classifier.Base(partit)
import Vision

main = do
    sz <- findSize
    file:_ <- getArgs
    prepare
    cam <- mplayer ("mf://"++file) sz
    img <- cam

    rightAngles (rgb $ channels img)

    mainLoop

--------------------------------------------------------------

rightAngles img = do
    wrect <- evWindow (ident 3) "Metric Rectification" (Size 600 600) (Just disp2) (const kbdQuit)
    worig <- evWindow [] "Image" (size img) (Just disp) (mouse wrect kbdQuit)
    return ()
    where
    disp2 st = do
        rec <- get st
        drawImage $ warp (0,0,0) (Size 600 600) (scaling 0.5 <> rec) img
    disp st = do
        drawImage img
        ps <- get st
        mapM_ (renderPrimitive LineStrip . mapM_ vertex) (partit 3 $ reverse ps)
        pointSize $= 3
        renderPrimitive Points $ mapM_ vertex ps
    mouse wrect _ st (MouseButton LeftButton) Down _ (Position x y) = do
        st $~ (Pixel (fromIntegral y) (fromIntegral x) :)
        ps <- get st
        putW wrect $ rectifier $ pixelsToPoints (size img) $ ps
        postRedisplay Nothing
        postRedisplay (Just $ evW wrect)
    mouse _ _ st (Char 'x') Down _ _ = do
        st $~ (\l -> if null l then l else tail l)
        postRedisplay Nothing
    mouse _ def _ a b c d = def a b c d

--------------------------------------------------------------

rectifier ps | length ps >= 15 && ok = rectifierFromAbsoluteDualConic omega
             | otherwise             = ident 3
    where
    omega = estimateAbsoluteDualConic pairlines
    pairlines = map p3l $ filter ((==3).length) $ partit 3 $ reverse ps
    p3l ps = (toList (cross p1 p2), toList (cross p2 p3))
        where [p1,p2,p3] = map p2vh ps
    ok = posdef omega
    p2vh (Point x y) = fromList [x,y,1]
    posdef mat = minimum (toList s) >= 0 where (s,_) = eigSH mat

-- zoom window: click to center, wheel to zoom

import EasyVision
import Graphics.UI.GLUT hiding (Size, Point)
import System(getArgs)
import Control.Monad(when)
import Numeric.LinearAlgebra
import Util.Misc(splitEvery)
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
    wrect <- evWindow ([],ident 3) "Metric Rectification" (Size 600 600) (Just disp2) (const kbdQuit)
    worig <- evWindow testpoints "Image" (size img) (Just disp) (mouse wrect kbdQuit)
    return ()
    where
    disp2 st = do
        (ps,rec) <- get st
        drawImage $ warp (0,0,0) (Size 600 600) rec img
        pointCoordinates (Size 600 600)
        mapM_ (renderPrimitive LineStrip . mapM_ vertex) (splitEvery 3 $ reverse ps)
    disp st = do
        drawImage img
        ps <- get st
        mapM_ (renderPrimitive LineStrip . mapM_ vertex) (splitEvery 3 $ reverse ps)
        pointSize $= 3
        renderPrimitive Points $ mapM_ vertex ps
    mouse wrect _ st (MouseButton LeftButton) Down _ (Position x y) = do
        st $~ (Pixel (fromIntegral y) (fromIntegral x) :)
        ps <- get st
        let pts = pixelsToPoints (size img) $ ps
            rec = rectifier pts
        putW wrect (htp rec pts, rec)
        postRedisplay Nothing
        postRedisplay (Just $ evW wrect)
    mouse _ _ st (Char 'x') Down _ _ = do
        st $~ (\l -> if null l then l else tail l)
        postRedisplay Nothing
    mouse _ def _ a b c d = def a b c d

--------------------------------------------------------------

rectifier ps = rec where
    rec = case estimateAbsoluteDualConic pairlines of
        Nothing -> ident 3
        Just omega -> relocate (rectifierFromAbsoluteDualConic omega)
    pairlines = map p3l $ filter ((==3).length) $ splitEvery 3 $ reverse ps
    p3l ps = (toList (cross p1 p2), toList (cross p2 p3))
        where [p1,p2,p3] = map p2vh ps
              p2vh (Point x y) = fromList [x,y,1]

    relocate h = r <> h where
        r = similarFrom2Points a b [0,1] [0,-1]
        [a,b] = ht h [[0,0.75],[0,-0.75]]

--diagl = diag . fromList
htp h = ht h . map p2l
p2l (Point x y) = [x,y]
--l2p [x,y] = Point x y

-------------------------------------------------------------------
testpoints= [Pixel {row = 452, col = 253},Pixel {row = 198, col = 392},Pixel {row = 223, col = 483},Pixel {row = 315, col = 391},Pixel {row = 304, col = 207},Pixel {row = 189, col = 242},Pixel {row = 355, col = 515},Pixel {row = 249, col = 191},Pixel {row = 430, col = 34},Pixel {row = 130, col = 335},Pixel {row = 231, col = 262},Pixel {row = 180, col = 101},Pixel {row = 241, col = 424},Pixel {row = 352, col = 33},Pixel {row = 101, col = 32},Pixel {row = 250, col = 593},Pixel {row = 169, col = 504},Pixel {row = 214, col = 327},Pixel {row = 315, col = 392},Pixel {row = 399, col = 133},Pixel {row = 221, col = 107}]

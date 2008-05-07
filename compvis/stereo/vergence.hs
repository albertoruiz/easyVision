
import EasyVision
import Graphics.UI.GLUT hiding (RGB,Size,Point)
import Numeric.LinearAlgebra hiding ((.*))
import qualified Numeric.LinearAlgebra as LA
import Vision
import Text.Printf(printf)

------------------------------------------------------------

main = do
    prepare
    sz <- findSize
    n <- numCams

    let vc c = c >>= withChannels
    cams <- mapM (vc . flip getCam sz) [0..n-1]

    mbf <- getRawOption "--calib"
    let file = case mbf of
                Nothing -> "cameras.txt"
                Just file -> file
    campars <- read `fmap` readFile file

    let (k,_,_) = factorizeCamera (campars!!0)

    wrectif <- evWindow (0,0,20,0.7) "rectif" sz Nothing (mouse kbdQuit sz)

    launch $ do

        (x,y,d,scale) <- getW wrectif

        im1 <- fmap (float.gray) (cams!!0)
        im2 <- fmap (float.gray) (cams!!1)

        let look = toList $ d LA..* inv k <> fromList [x,y,1]
            rec1 = virtualLook (campars!!0) look
            rec2 = virtualLook (campars!!1) look
            w1 = warp 0 (size im1) (scaling scale <> rec1) im1
            w2 = warp 0 (size im2) (scaling scale <> rec2) im2

        inWin wrectif $ do
            drawImage (0.5 .* w1 |+| 0.5 .* w2)
            pointCoordinates sz
            setColor 1 1 0
            lineWidth $= 1
            renderAxes
            pixelCoordinates sz
            setColor 1 1 1
            text2D 20 20 (printf "%.1f cm" (10*d))



virtualLook cam [x,y,z] = rot
    where (k,r,c) = factorizeCamera cam
          [cx,cy,cz] = toList c
          [r1,r2,r3] = toColumns r
          r3' = unitary (fromList [x,y,z] - c)
          r1' = unitary (r2 `cross` r3')
          r2' = unitary (r3' `cross` r1')
          r' = fromColumns [r1',r2',r3']
          [a,b,_] = toList $ r' <> fromList [0,1,0]
          roll = atan2 a b
          rot = rot3 roll <>  k <> trans r' <> trans r <> inv k



mouse _ _ st (MouseButton WheelUp) Down Modifiers {shift=Up} _ = do
    (x,y,d,s) <- get st
    st $= (x,y,d*1.1,s)
mouse _ _ st (MouseButton WheelDown) Down Modifiers {shift=Up} _ = do
    (x,y,d,s) <- get st
    st $= (x,y,d/1.1,s)
mouse _ _ st (MouseButton WheelUp) Down Modifiers {shift=Down} _ = do
    (x,y,d,s) <- get st
    st $= (x,y,d,s/1.1)
mouse _ _ st (MouseButton WheelDown) Down Modifiers {shift=Down} _ = do
    (x,y,d,s) <- get st
    st $= (x,y,d,s*1.1)
mouse _ sz st (MouseButton LeftButton) Down _ pos@(Position c r) = do
    (x,y,d,s) <- get st
    let [Point dx dy] = pixelsToPoints sz [Pixel (fromIntegral r) (fromIntegral c)]
    st $= (dx/s+x,dy/s+y,d,s)
mouse _ _ st (Char 'x') Down _ _ = do
    st $= (0,0,20,0.7)
mouse def _ _ a b c d = def a b c d


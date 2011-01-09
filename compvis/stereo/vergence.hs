
import EasyVision hiding (shift)
import Graphics.UI.GLUT hiding (RGB,Size,Point)
import Numeric.LinearAlgebra hiding ((.*))
import qualified Numeric.LinearAlgebra as LA
import Vision
import Text.Printf(printf)
import Util.Misc(unitary)
import Util.Rotation(rot3)

------------------------------------------------------------

main = do
    prepare
    sz <- findSize
    n <- numCams

    let vc c = c ~> channels
    cams <- mapM (vc . flip getCam sz) [0..n-1]

    mbf <- getRawOption "--calib"
    let file = case mbf of
                Nothing -> "cameras.txt"
                Just file -> file
    campars <- read `fmap` readFile file

    let (k,_,_) = factorizeCamera (campars!!0)
        [c0,c1] = map (inHomog.nullVector) campars

    wrectif <- evWindow (0,0,20,1) "rectif" sz Nothing (mouse kbdQuit sz)

    o <- createParameters [("umb",intParam 128 1 255),
                           ("rad",intParam 30 10 100),
                           ("sig",realParam 2 0.1 10)
                          ]

    let p_umb = getParam o "umb" :: IO Int
        p_rad = getParam o "rad" :: IO Int
        p_sig = getParam o "sig" :: IO Float

    launch $ do

        (x,y,d,scale) <- getW wrectif

        umb <- p_umb
        rad <- p_rad
        sig <- p_sig

        im1 <- fmap gray (cams!!0)
        im2 <- fmap gray (cams!!1)

        let look = [d*x,d*y,d]
            rec1 = virtualLook (campars!!0) look
            rec2 = virtualLook (campars!!1) look
            d0 = norm (vector look - c0)
            d1 = norm (vector look - c1)

            proc = gaussS sig . float . highPass8u Mask5x5 -- = binarize8u (fromIntegral umb) True . highPass8u Mask5x5

            w1 = proc $ warp 0 (size im1) (scaling scale <> rec1) im1
            w2 = proc $ warp 0 (size im2) (scaling (d1/d0*scale) <> rec2) im2

            roi = roiFromPixel rad orig where [orig] = pointsToPixels sz [Point 0 0]

            disc = rawDisc (modifyROI (const roi) w1) (modifyROI (const roi) w2)

        inWin wrectif $ do
            drawImage (0.5 .*  w1 |+| 0.5 .*  w2)
            pointCoordinates sz
            setColor 1 1 0
            lineWidth $= 1
            renderAxes
            pixelCoordinates sz
            setColor 1 1 1
            text2D 20 20 (printf "%.1f cm" (10*d))
            text2D 20 40 (printf "%.2f" disc)
            drawROI roi




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
          rot = rot3 roll <> trans r' <> trans r <> inv k



mouse _ _ st (MouseButton WheelUp) Down Modifiers {shift=Up} _ = do
    (x,y,d,s) <- get st
    st $= (x,y,d*1.05,s)
mouse _ _ st (MouseButton WheelDown) Down Modifiers {shift=Up} _ = do
    (x,y,d,s) <- get st
    st $= (x,y,d/1.05,s)
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
    st $= (0,0,20,1)
mouse def _ _ a b c d = def a b c d

vector l = fromList l :: Vector Double

norm = pnorm PNorm2

rawDisc im1 im2 = sum32f (abs32f (im1 |-| im2)) / fromIntegral (roiArea $ theROI im1)

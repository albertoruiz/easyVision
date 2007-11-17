
import EasyVision
import Graphics.UI.GLUT hiding (RGB,Size,Point)
import qualified Graphics.UI.GLUT as GL
import Data.IORef
import System.Exit
import System.Environment(getArgs)
import GHC.Float(isDoubleNaN)
import ImagProc.Ipp.Core
import Data.List(minimumBy)
import Numeric.LinearAlgebra hiding ((.*))
import Control.Monad(when)
import Vision
import GHC.Float(float2Double,double2Float)
import Classifier.Stat
import qualified Data.Map as Map

------------------------------------------------------------

pointTracker sz cam = do
    (cam',ctrl) <- withPause cam
    ref <- newIORef Nothing
    rmarked <- newIORef []
    w <- evWindow () "tracker" sz (Just (const $ drw ref rmarked)) (marker (rmarked,ref) (kbdcam ctrl))
    opts <- createParameters     [ ("h",percent 20)
                                 , ("locrad",intParam 3 1 30)
                                 , ("smooth",intParam 3 0 10)
                                 , ("median",intParam 0 0 10)
                                  --("rad", realParam 7 1 20),
                                  --("dim", intParam 20 1 50)
                                 ]
    return $ do
        ph <- getParam opts "h" :: IO Int
        let h = fromIntegral ph / 100
        smooth <- getParam opts "smooth"
        --rad <- getParam opts "rad"
        --dim <- getParam opts "dim"
        locrad <- getParam opts "locrad"
        meds <- getParam opts "median" :: IO Int
        let diam = 2*locrad+1
        img <- cam' >>= yuvToGray >>= meds `times` (median Mask3x3) >>= scale8u32f 0 1 
        --ips <- getSaddlePoints smooth locrad h 300 dim rad img
        ips <- getCorners smooth locrad h 300 img
        writeIORef ref (Just (img,ips))
        currentWindow $= Just (evW w)
        postRedisplay Nothing
        return $ (img,ips)
  where drw ref rmarked = do
            mb <- readIORef ref
            case mb of
                Nothing -> return ()
                Just (img,ips) -> do
                    drawImage img
                    pixelCoordinates (size img)
                    pointSize $= 4
                    setColor 0.5 0.5 1
                    renderPrimitive Points $ mapM_ vertex ips
                    --drawInterestPoints (size img) ips
                    marked <- readIORef rmarked
                    pointSize $= 2
                    setColor 1 0 0
                    renderPrimitive Points $ mapM_ vertex marked

        marker (rmarked,ref) _ _ (MouseButton LeftButton) Down _ pos@(Position x y) = do
            marked <- readIORef rmarked
            let clicked = Pixel (fromIntegral y) (fromIntegral x)
            Just (_, pts) <- readIORef ref
            let new = closest pts clicked
            writeIORef rmarked (new:marked)
        marker (rmarked,_) _ _ (MouseButton RightButton) Down _ pos@(Position x y) = do
            marked <- readIORef rmarked
            when (not (null marked)) $ writeIORef rmarked (tail marked)
        marker _ def _ a b c d = def a b c d

        closest [] p = p
        closest hp p = minimumBy (compare `on` dist p) hp
            where dist (Pixel a b) (Pixel x y) = (a-x)^2+(b-y)^2

------------------------------------------------------------

main = do
    sz <- findSize

    state <- prepare' ()

    opts <- createParameters        [("umb",realParam 0.01 0 0.05),
                                    ("ranUmb", realParam 0.003 0 0.01),
                                    ("scale", realParam 0.5 0 2),
                                    ("ranProb", realParam 0.9 0.5 1),
                                    ("seeRansac", intParam 0 0 1)]

    (cam,ctrl) <- getCam 0 sz >>= pointTracker sz >>= withPause

    launch' state (worker cam opts)

-----------------------------------------------------------------


worker cam opts inWindow _ = do

    cam

    return ()

closestBy f [] p = p
closestBy f hp p = minimumBy (compare `on` f p) hp

norm x = pnorm PNorm2 x

distFeat = (dist `on` ipDescriptor)
    where dist u v = norm (u-v)/ fromIntegral (dim u)

distSpatial = (dist `on` ipPosition)
    where dist (Point a b) (Point x y) = sqrt $ abs ((a-x)^2+(b-y)^2)

distComb alpha p q = chk $ alpha*distFeat p q + (1-alpha)*distSpatial p q

chk x | isDoubleNaN x == 0 = x 
      | otherwise          = 500 -- error "NaN"
-----------------------------------------------------------

prep = map (g.ipPosition) where g (Point x y) = [x,y]

----------------------------------------------------------------------------

mean l = sum l / fromIntegral (length l)

combine f (img1,pts1) (img2,pts2) = do
    let img = 0.5 .* img1 |+| 0.5 .* img2
    drawImage img
    setColor 1 0 0
    lineWidth $= 1
    pointCoordinates (size img1)
    renderPrimitive Lines (mapM_ drawSeg (zipWith Segment (map f pts1) (map f pts2)))

drawSeg s = do
    vertex $ (extreme1 s)
    vertex $ (extreme2 s)

text2D x y s = do
    rasterPos (Vertex2 x (y::GLfloat))
    renderString Helvetica12 s


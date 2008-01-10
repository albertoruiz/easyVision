-- tracking a blue region

import EasyVision hiding (State)
import Control.Monad((>=>),when)
import Graphics.UI.GLUT hiding (Point,Matrix,matrix,Size,triangulate)
import Data.List
import ImagProc.Ipp.Core
import Numeric.LinearAlgebra
import Data.IORef
import Vision


colorDetector "" cam = do
    o <- createParameters [("kb",intParam 60  0 255),
                           ("kg",intParam 100 0 255),
                           ("kw",intParam 200 0 255)]
    return $ do
        kb <- fromIntegral `fmap` (getParam o "kb" :: IO Int)
        kg <- fromIntegral `fmap` (getParam o "kg" :: IO Int)
        kw <- fromIntegral `fmap` (getParam o "kw" :: IO Int)
        orig <- cam
        let col = detectRange 7 8 . hsvCode kb kg kw . hsv $ orig
            rawconts = map fst3 $ take 1 $ sortBy (compare `on` (negate.fst2)) $ contours 5 200 1 True col
            conts = map (momentsContour.pixelsToPoints (size col)) rawconts
            p = case conts of
                    [] -> Nothing
                    [(x,y,_,_,_)] -> Just (Point x y)
        return (orig, p)


colorDetector name cam = do
    e <- evWindow () name (mpSize 8) Nothing (const kbdQuit)
    cam' <- colorDetector "" cam
    return $ do
        (orig,p) <- cam'
        inWin e $ do
            drawImage (rgb orig)
            setColor 1 1 1
            pointSize $= 5
            pointCoordinates (size (rgb orig))
            case p of
                Just p -> renderPrimitive Points $ mapM_ vertex [p]
                Nothing -> return ()
        return (orig, p)

detectRange a b im = purifyWith (set8u 0) $
    thresholdVal8u a 0 IppCmpLess im >>=
    thresholdVal8u b 0 IppCmpGreater

fst3 (a,_,_) = a
fst2 (_,a,_) = a


stabilize "" detector = do
    r <- newIORef s0
    return $ do
        (orig,p) <- detector
        st <- get r
        let st'@(State x c) =
                case p of
                    Nothing -> blind sys st
                    Just (Point x y)  -> kalman sys st (vector [x, y])
        r $= st'
        let pt = Point (x@>0) (x@>1)
            v = (x@>2,x@>3)
        return (orig, (pt,v))

stabilize name detector = do
    det <- stabilize "" detector
    e <- evWindow () "kalman" (mpSize 8) Nothing (const kbdQuit)
    return $ do
        (orig, (pt@(Point x y),v@(vx,vy))) <- det
        let pt2 = Point (x+vx) (y+vy)
        inWin e $ do
            drawImage (rgb orig)
            setColor 1 1 1
            pointSize $= 5
            pointCoordinates (size (rgb orig))
            renderPrimitive Points $ mapM_ vertex [pt]
            setColor 1 0 0
            lineWidth $= 2
            renderPrimitive Lines $ mapM_ vertex [pt,pt2]
        return (orig, (pt,v))


main = do
    sz <- findSize
    prepare

    (cam0,ctrl0) <- getCam 0 sz >>= withChannels >>= colorDetector "" >>= stabilize "Kalman" >>= withPause
    (cam1,ctrl1) <- getCam 1 sz >>= withChannels >>= colorDetector "" >>= stabilize "Kalman" >>= withPause

    (trackball,kc,mc) <- newTrackball
    w3D <- evWindow True "3D view" (Size 400 400) Nothing (const $ kc kbdQuit)
    --keyboardMouseCallback $= Just (kc (kbdcam ctrl))
    motionCallback $= Just mc
    depthFunc $= Just Less
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureFunction $= Replace

    launch $ inWin w3D $ do
       clear [ColorBuffer, DepthBuffer]
       trackball
       (orig0,(p1,v1)) <- cam0
       (orig1,(p2,v2)) <- cam1

       setColor 1 1 1
       imt0 <- extractSquare 128 (float $ gray $ orig0)
       let c0 = cameraAtOrigin
       drawCamera 1 c0 (Just imt0)

       imt1 <- extractSquare 128 (float $ gray $ orig1)
       let c1 = (3><4) [ 1, 0, 0, 5,
                         0, 1, 0, 0,
                         0, 0, 1, 0 ]
       drawCamera 1 c1 (Just imt1)

       let Point x1 y1 = p1
           Point x2 y2 = p2
           [[x,y,z]] = triangulate [(c0, [[x1,y1]]), (c1, [[x2,y2]])]

       setColor 0.5 0.5 0.5
       renderPrimitive LineStrip $ mapM_ vertex [[0,0,0],[x,y,z],[-5,0,0]]
       setColor 0 0 1
       pointSize $= 5
       renderPrimitive Points $ mapM_ vertex [[x,y,z]]


       wasMoving <- getW w3D

       let stopped = norm v1 < 1/640 && norm v2 < 1/640
           moveAgain = norm v1 > 5/640 || norm v2 > 5/640

       when (wasMoving && stopped) $ do
            print (p1,p2)
            putW w3D False

       when moveAgain $ do
            putW w3D True


-- Kalman filter for 2D position and velocity

vector l = fromList l :: Vector Double
matrix ls = fromLists ls :: Matrix Double
diagl = diag . vector

f = matrix [[1,0,1,0],
            [0,1,0,1],
            [0,0,1,0],
            [0,0,0,1]]

h = matrix [[1,0,0,0],
            [0,1,0,0]]

q = 1 * diagl [1,1,1,1]

r = 10 * diagl [1,1]

s0 = State (vector [0, 0, 0, 0]) (diagl [1, 1, 1, 1])

sys = System f h q r

data System = System {kF, kH, kQ, kR :: Matrix Double}
data State = State {sX :: Vector Double , sP :: Matrix Double} deriving Show
type Measurement = Vector Double

kalman :: System -> State -> Measurement -> State
kalman (System f h q r) (State x p) z = State x' p' where
    px = f <> x                            -- prediction
    pq = f <> p <> trans f + q             -- its covariance
    y  = z - h <> px                       -- residue
    cy = h <> pq <> trans h + r            -- its covariance
    k  = pq <> trans h <> inv cy           -- kalman gain
    x' = px + k <> y                       -- new state
    p' = (ident (dim x) - k <> h) <> pq    -- its covariance


blind :: System -> State -> State
blind (System f h q r) (State x p) = State x' p' where
    x' = f <> x
    p' = f <> p <> trans f + q

norm (vx,vy) = sqrt $ vx * vx + vy*vy

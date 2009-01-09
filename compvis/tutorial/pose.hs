import EasyVision
import Vision
import Numeric.LinearAlgebra
import System.Environment(getArgs)
import Graphics.UI.GLUT hiding (Point,Size,scale)
import Control.Monad(when)

main = do
    prepare
    sz <- findSize
    mbf <- maybeOption "--focal"
    (cam,ctrl) <- getCam 0 sz ~> gray.channels >>= withPause
    wIm <- evWindow () "image" sz Nothing (const $ kbdcam ctrl)
    w3D <- evWindow3D () "3D view" 400 (const (kbdcam ctrl))
    launch $ do
        orig <- cam
        let segs = filter ((>0.1).segmentLength) $
                       segments 4 1.5 5 40 20 True (autoBin orig)
            polis = segmentsToPolylines 0.06 segs
            closed = [p | Closed p <- polis, length p == length ref]
            refs = filter (isRef mbf 0.3) (concatMap alter closed)
        inWin wIm $ do
            drawImage orig
            pointCoordinates (size orig)
            setColor 0 0 1; lineWidth $= 1; renderPrimitive Lines $ mapM_ drawSeg segs
            setColor 1 0 0; lineWidth $= 3; mapM_ (renderAs LineLoop) closed
            setColor 0 1 0; pointSize $= 5; mapM_ (renderAs Points) closed
        inWin w3D $ do
            setColor 0 0 1; lineWidth $= 2; renderAs LineLoop ref
            when (not.null $ refs) $ do
                let pts = map pl (head refs)
                    h = estimateHomography ref pts
                    imf = float orig
                    ground = warp 0 (Size 256 256) (scaling 0.2 <> h) imf
                    groundpos = ht (scaling (1/0.2)) [[1,1],[-1,1],[-1,-1],[1,-1]]
                drawTexture ground $ map (++[-0.01]) groundpos
                let imt = extractSquare 128 imf
                    Just (cam,_) = cameraFromPlane 1E-3 500 mbf pts ref
                drawCamera 1 cam (Just imt)
                pointCoordinates (Size 400 400); setColor 1 1 1
                text2D 0.95 (-0.95) (show $ focalFromHomogZ0 $ inv h)

--ref = [[0,0],[0,2.97],[2.10,2.97],[2.10,0]]
ref = map (map (*2)) cornerRef

pl (Point x y) = [x,y]
alter pts = map (rotateList pts) [0 .. length ref -1]
rotateList list n = take (length list) $ drop n $ cycle list
drawSeg s = (vertex $ extreme1 s) >> (vertex $ extreme2 s)
autoBin img = binarize8u (otsuThreshold img) True img
renderAs prim = renderPrimitive prim . (mapM_ vertex)

isRef mbf tol pts = ao < tol -- && cy < 0
    where mbomega = fmap omegaGen mbf
          ao = autoOrthogonality mbomega h
          h = estimateHomography (map pl pts) ref
          Just p = poseFromHomogZ0 mbf h
          (_,cy,_) = cameraCenter p
          omegaGen f = kgen (recip (f*f))

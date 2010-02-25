import EasyVision
import ImagProc.C.Segments
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
        let segs = filter ((>0.1).segmentLength) $ segments 4 1.5 5 40 20 True (autoBin orig)
            polis = segmentsToPolylines 0.06 segs
            candis = concat [ alter p | Closed p <- polis, length p == length ref]
            refs = filter (isRef 0.01) candis
            pts = map pl (head refs)
            h = estimateHomography ref pts
            imf = float orig
            ground = warp 0 (Size 256 256) (scaling 0.2 <> h) imf
            grpos = ht (scaling (1/0.2)) [[1,1],[-1,1],[-1,-1],[1,-1]]
            imt = extractSquare 128 imf
            Just (cam,_) = cameraFromPlane 1E-3 500 mbf pts ref
        inWin wIm $ do
            drawImage orig
            pointCoordinates (size orig)
            setColor 0 0 1; lineWidth $= 1; renderSegments segs
            setColor 1 0 0; lineWidth $= 3; mapM_ (renderAs LineLoop) candis
            setColor 0 1 0; pointSize $= 5; mapM_ (renderAs Points) candis
        when (not.null $ refs) $ inWin w3D $ do
            setColor 0 0 1; lineWidth $= 2; renderAs LineLoop ref
            drawTexture ground $ map (++[-0.01]) grpos
            drawCamera 1 cam (Just imt)
            pointCoordinates (Size 400 400); setColor 1 1 1
            text2D 0.95 (-0.95) (show $ focalFromHomogZ0 $ inv h)

ref = map (map (*2)) cornerRef
pl (Point x y) = [x,y]
alter pts = map (rotateList pts) [0 .. length ref -1]
rotateList list n = take (length list) $ drop n $ cycle list
drawSeg s = (vertex $ extreme1 s) >> (vertex $ extreme2 s)
autoBin img = binarize8u (otsuThreshold img) img
renderAs prim = renderPrimitive prim . (mapM_ vertex)
renderSegments segs = renderPrimitive Lines $ mapM_ drawSeg segs

isRef tol pts = dif < tol where
    lps = (map pl pts)
    h = estimateHomography lps ref
    lps' = ht h ref
    dif = pnorm Infinity $ flatten (fromLists lps - fromLists lps')

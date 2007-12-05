-- planar metric rectification from a rectangle with arbitrary aspect ratio
-- example:
-- ./rectangles consis002.dv --size 12

module Main where

import EasyVision
import Graphics.UI.GLUT hiding (Matrix, Size, Point)
import Vision
import Control.Monad(when)
import Numeric.LinearAlgebra

main = do
    sz <- findSize

    (cam,ctrl) <- getCam 0 sz >>= withPause

    prepare

    o <- createParameters     [("radius",intParam 4 0 10),
                               ("width",realParam 1.5 0 5),
                               ("median",intParam 5 3 5),
                               ("high",intParam 40 0 255),
                               ("low",intParam 20 0 255),
                               ("postproc",intParam 1 0 1),
                               ("minlength",realParam 0.15 0 1),
                               ("maxdis",realParam 0.06 0 0.1),
                               ("scale",realParam 0.2 0.01 1),
                               ("orthotol",realParam 0.25 0.01 0.5)]

    wIm <- evWindow () "image" (Size 400 400) Nothing (const $ kbdcam ctrl)

    wRec <- evWindow () "rectified" (Size 400 400) Nothing (const $ kbdcam ctrl)

    launch (worker wIm wRec cam o)

-----------------------------------------------------------------


worker wIm wRec cam op = do

    radius <- getParam op "radius"
    width  <- getParam op "width"
    median <- getParam op "median"
    high   <- fromIntegral `fmap` (getParam op "high" :: IO Int)
    low    <- fromIntegral `fmap` (getParam op "low" :: IO Int)
    postp  <- getParam op "postproc" :: IO Int
    let pp = if postp == 0 then False else True
    minlen <- getParam op "minlength"
    maxdis <- getParam op "maxdis"
    scale  <- getParam op "scale"
    orthotol  <- getParam op "orthotol" :: IO Double

    orig <- cam >>= yuvToGray
    let segs = filter ((>minlen).segmentLength) $ segments radius width median high low pp orig
        polis = segmentsToPolylines maxdis segs
        closed4 = [p | Closed p <- polis, length p == 4]
        (fs,recs) = unzip $ map (calibFromRectangle.construct) closed4

    inWin wIm $ do
        scale8u32f 0 1 orig >>= return . warp 0 (Size 400 400) (scaling scale) >>= drawImage

        pointCoordinates (Size 400 400)

        let drwclosed4 = map (scp scale) closed4

        setColor 1 0 0
        lineWidth $= 1
        mapM_ (renderPrimitive LineLoop . (mapM_ vertex)) drwclosed4

        setColor 1 0 0
        lineWidth $= 1
        mapM_ (renderPrimitive Lines . (mapM_ vertex)) (map construct drwclosed4)

        setColor 0 1 0
        pointSize $= 2
        mapM_ (renderPrimitive Points . (mapM_ vertex)) drwclosed4

        setColor 1 1 1
        text2D 0.95 (-0.95) (show fs)


    when (length recs >0) $ inWin wRec $ do
        let rectif = head recs
            rectangle = (head closed4)
            centRec = centerRect rectangle
            [dx,dy] = toList $ inHomog $ rectif <> centRec
            [a',b',c',d'] = ht rectif (map p2l rectangle)
            sc = scale/0.2*0.5/norm (vector a'- vector c')
            aspectRatio = norm (vector a'- vector d')/ norm (vector a'- vector b')

        scale8u32f 0 1 orig >>= return . warp 0 (Size 400 400) (scaling sc <> desp (-dx,-dy) <> rectif) >>= drawImage
        pointCoordinates (Size 400 400)
        setColor 1 1 1
        text2D 0.95 (-0.95) (show $ max aspectRatio (1/aspectRatio))

    return ()

---------------------------------------------------------

vector l = fromList l :: Vector Double

diagl = diag .vector

norm x = pnorm PNorm2 x

p2l (Point x y) = [x,y]
p2hp (Point x y) = vector [x,y,1]
hp2p v = Point x y where [x,y] = toList (inHomog v)

construct [a,b,c,d] = [Segment p a, Segment p b, 
                       Segment q b, Segment q c,
                       Segment p q, Segment (Point 0 0) n]
    where lab = cross (p2hp a) (p2hp b)
          lcd = cross (p2hp c) (p2hp d)
          lad = cross (p2hp a) (p2hp d)
          lbc = cross (p2hp b) (p2hp c)
          ph = cross lad lbc
          p = hp2p ph
          qh = cross lab lcd
          q = hp2p qh
          h = cross ph qh
          dn = mS <> h
          l0n = cross dn (vector [0,0,1])
          nh = cross h l0n
          n = hp2p nh

scp sc = map (\(Point x y)-> Point (sc*x) (sc*y))

calibFromRectangle [_,_,_,_,Segment p q, Segment z n@(Point x y)] = (f, rec)
    where x1  = segmentLength $ Segment p n
          x2  = segmentLength $ Segment q n
          yh   = segmentLength $ Segment z n
          f   = sqrt $ x1*x2-yh^2
          rho = atan2 x y
          rec = rectifier rho yh f

rectifier rho yh f = kgen f <> rot1 (atan2 f yh) <> rot3 (-rho) <> kgen (recip f)

centerRect [a,b,c,d] = cent
    where lac = cross (p2hp a) (p2hp c)
          lbd = cross (p2hp b) (p2hp d)
          cent = cross lac lbd

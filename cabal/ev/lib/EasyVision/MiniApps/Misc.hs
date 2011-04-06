-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApps.Misc
Copyright   :  (c) Alberto Ruiz 2006-11
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional

Additional camera combinators.

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps.Misc (
  addSmall,
  warper,
  findPolygons, getPolygons, polyconsis,
  findRectangles,
  onlyRectangles,
  rectifyQuadrangle
)where

import ImagProc.Ipp.Core
import ImagProc
import Features.Polyline
import Foreign
import EasyVision.GUI
import Graphics.UI.GLUT hiding (Size,Point)
import Vision
import Util.Rotation
import Util.Misc(degree)
import Vision.Autofrontal(autoOrthogonality)
import Numeric.LinearAlgebra hiding (join)
import Features.Segments

type SegmentExtractor = Int
         -> Float
         -> Int
         -> CUChar
         -> CUChar
         -> Bool
         -> ImageGray
         -> [Segment]

----------------------------------------------------------------------

-- | Given a camera adds a gray version (typically small), to be used with other combinators like 'detectMov'
addSmall :: Size -> (a->ImageGray) -> IO a -> IO (IO (a, ImageGray))
addSmall sz f grab = return $ do
    im <- grab
    let x = resize sz (f im)
    return (im,x)

---------------------------------------------------------

conjugateRotation pan tilt rho foc sca =
        scaling sca
        <> kgen foc
        <> rot1 tilt
        <> rot2 pan 
        <> rot3 rho 
        <> kgen (1/foc)

warper sz name = do
    param <- createParameters   [ ("pan",  realParam (0) (-40) (40))
                                 ,("tilt", realParam (0) (-30) (30))
                                 ,("rho",  realParam  0 (-60) (60))
                                 ,("foc",  listParam 2.8 [0.5, 0.7, 1, 2, 2.6, 2.8, 5, 5.5, 9,10])
                                 ,("sca",  listParam 1 [1.1**k|k<-[-20..20]])]
    let h = do
            pan   <- getParam param "pan"
            tilt  <- getParam param "tilt"
            rho   <- getParam param "rho"
            foc   <- getParam param "foc"
            sca   <- getParam param "sca"
            let t = conjugateRotation (pan*degree) (tilt*degree) (rho*degree) foc sca
            return t
        f img = do
            t <- h
            return $ warp (0::CUChar) sz t img

    let drw w img = do
        inWin w $ do
            windowStatus $= Shown
            f img >>= drawImage

    w <- evWindow undefined name sz Nothing (const kbdQuit)
    windowStatus $= Hidden
    putW w (h,drw w)
    return w

----------------------------------------------------------------

-- A virtual camera which finds rectangles with a given aspect ratio

findRectangles segments ratio cam = do
    op <- createParameters [ ("radius",intParam 4 0 10),
                             ("width",realParam 1.5 0 5),
                             ("median",intParam 5 3 5),
                             ("high",intParam 40 0 255),
                             ("low",intParam 20 0 255),
                             ("postproc",intParam 1 0 1),
                             ("minlength",realParam 0.15 0 1),
                             ("maxdis",realParam 0.06 0 0.1),
                             ("orthotol",realParam 0.4 0.01 1.0)]
    let a4 = [[   0,            0]
            ,[   0, (2.10*ratio)]
            ,[2.10, (2.10*ratio)]
            ,[2.10,           0]]
    return $ do
        orig <- cam
        let img = gray orig
        radius <- getParam op "radius"
        width  <- getParam op "width"
        median <- getParam op "median"
        high   <- fromIntegral `fmap` (getParam op "high" :: IO Int)
        low    <- fromIntegral `fmap` (getParam op "low" :: IO Int)
        postp  <- getParam op "postproc" :: IO Int
        let pp = if postp == 0 then False else True
        minlen <- getParam op "minlength"
        maxdis <- getParam op "maxdis"
        orthotol  <- getParam op "orthotol"
        let
            alter pts = map (rotateList pts) [0 .. 3]
            mbf = Nothing
            segs = filter ((>minlen).segmentLength) $ (segments::SegmentExtractor) radius width median high low pp img
            polis = segmentsToPolylines maxdis segs
            closed4 = [p | Closed p <- polis, length p == 4]
            a4s = filter (isA4 mbf orthotol a4) (concatMap alter closed4)
        return (orig,a4s)


isA4 mbf tol a4 pts = ao < tol && cy < 0
    where mbomega = fmap omegaGen mbf
          ao = autoOrthogonality mbomega h
          h = estimateHomography (map pl pts) a4
          Just p = poseFromHomogZ0 mbf h
          (_,cy,_) = cameraCenter p
          omegaGen f = kgen (recip (f*f))

----------------------------------------------------------------

onlyRectangles segments sz ratio sel cam = do
    fr <- findRectangles segments ratio cam
    return $ do
        (orig,a4s) <- fr
        let f pts = fst . rectifyQuadrangle sz pts . sel $ orig
        return $ map f a4s

------------------------------------------------------------------

-- | convenience function
--rectifyQuadrangle :: Size -> [Point] -> ImageFloat -> (ImageFloat, Matrix Double)
rectifyQuadrangle sz pts imf = (r,h) where
    a4aux = [[-1,-r],[1,-r],[1,r],[-1,r]]
        where r = 1/ratio
              Size h w = sz
              ratio = fromIntegral w / fromIntegral h
    h = estimateHomography a4aux (map pl pts)
        where pl (Point x y) = [x,y]
    r = warp' sz h imf

-- initialization not required
-- FIXME
warp' s h im = unsafePerformIO $ do
    r <- image s
    warpOn' h r im
    return r

------------------------------------------------------------------------

-- A camera combinator which finds a given polygon and gives its pose
findPolygons :: SegmentExtractor -> Maybe Double -> [[Double]] -> IO (Channels) -> IO (IO(Channels,[([Point],CameraParameters)]))
findPolygons segments mbf ref cam = do
    op <- createParameters [ ("radius",intParam 4 0 10),
                             ("width",realParam 1.5 0 5),
                             ("median",intParam 5 3 5),
                             ("high",intParam 40 0 255),
                             ("low",intParam 20 0 255),
                             ("postproc",intParam 1 0 1),
                             ("minlength",realParam 0.15 0 1),
                             ("maxdis",realParam 0.06 0 0.1),
                             ("orthotol",realParam 0.4 0.01 1.0)]
    return $ do
        orig <- cam
        let img = gray orig
        radius <- getParam op "radius"
        width  <- getParam op "width"
        median <- getParam op "median"
        high   <- fromIntegral `fmap` (getParam op "high" :: IO Int)
        low    <- fromIntegral `fmap` (getParam op "low" :: IO Int)
        postp  <- getParam op "postproc" :: IO Int
        let pp = if postp == 0 then False else True
        minlen <- getParam op "minlength"
        maxdis <- getParam op "maxdis"
        orthotol  <- getParam op "orthotol"
        let oks = getPolygons' segments radius width median high low pp minlen maxdis orthotol mbf ref img
        return (orig,oks)

pl (Point x y) = [x,y]

rotateList list n = take (length list) $ drop n $ cycle list

-- (to do: normalize or pass as parameter costHomog value)
consistency mbf tol ref pts = (okpose && ao < tol && costHomog pts ref p < 1E-1 * perim, (pts,p)) -- && cy < 0
    where mbomega = fmap omegaGen mbf
          ao = autoOrthogonality mbomega h
          h = estimateHomography (map pl pts) ref
          pose = poseFromHomogZ0 mbf h
          okpose = isJust pose
          isJust Nothing = False
          isJust _       = True
          Just p = pose
          (_,cy,_) = cameraCenter p
          omegaGen f = kgen (recip (f*f))
          perim = perimeter (Closed pts)

costHomog view world c = pnorm PNorm1 $ flatten (fromLists (map pl view) -
                                        htm  (syntheticCamera c) (fromLists $ map (++[0]) world))

-- the essential function used by findPolygons. TO DO: move to a more appropriate module
getPolygons :: SegmentExtractor -> Maybe Double -> [[Double]] -> ImageGray -> [([Point],CameraParameters)]
getPolygons segments = getPolygons' segments 4 1.5 5 40 20 True 0.05 0.06 0.4

getPolygons' segments radius width median high low pp minlen maxdis orthotol mbf ref img = oks where
    l = length ref
    alter pts = map (rotateList pts) [0 .. l-1]
    segs = filter ((>minlen).segmentLength) $ segments radius width median high low pp img
    polis = segmentsToPolylines maxdis segs
    candis = [p | Closed p <- polis, length p == l]
    poses = map (consistency mbf orthotol ref) (concatMap alter candis)
    oks = map snd (filter fst poses)


polyconsis mbf orthotol ref polys = oks
    where poses = map (consistency mbf orthotol ref) (concatMap alter candis)
          oks = map snd (filter fst poses)
          alter pts = map (rotateList pts) [0 .. l-1]
          l = length ref
          candis = filter ((==l).length) polys


-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.PoseTracker
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Some variations of UKF pose tracker.

-}
-----------------------------------------------------------------------------

module EasyVision.PoseTracker (
    poseTracker
)where


import Graphics.UI.GLUT as GL hiding (Size,Point,Matrix,matrix)
import EasyVision.GUI hiding (State)
import ImagProc hiding ((.*))
import Control.Monad(when)
import Data.List(sort,nub,sortBy,minimumBy)
import EasyVision.Util
import Numeric.LinearAlgebra
import Data.IORef
import Vision
import EasyVision.Combinators(findPolygons,getPolygons)
import Kalman
import Text.Printf
import Classifier.Stat

vector l = fromList l :: Vector Double
diagl = diag . vector
disp x = putStrLn . format " " (printf "%.0f") $ x
log10 x = log x / log 10


--------------------------------------- pose tracker ---------------------------------

-- Unscented Kalman filter for pose tracking from a planar reference
poseTracker :: String -> Maybe Double -> [[Double]] -> IO Channels
            -> IO (IO(Channels, CameraParameters, (Vector Double, Matrix Double), Maybe (Vector Double, Double)))

poseTracker "" mbf ref cam = do
    tracker <- poseTrackerGen (withRegion ref) mbf ref
    return $ do
        img <- cam
        ((pose,st,cov),obs) <- tracker (gray img)
        return (img, pose, (st,cov), obs)

poseTracker winname mbf ref cam = poseTrackerMonitor poseTracker winname mbf ref cam


---------------------------------------------------------------------------------

systemNoise = map (/1000) [0,0,0,0,0,0,0.1,0.1,0.1,0.01,0.01,0.01]

poseTrackerGen (measure,post,cz) Nothing world = generalTracker st0 cov0 restart measure f cs h cz user
    where st0 = vector $ (cam2list $ easyCamera 40 (0,0,5) (0,0,0) 0) ++ [0,0,0,0,0,0]
          cov0 = diagl [3,1,1,1,10,10,10,10,10,10,5,5,5]
          f [foc,p,t,r,cx,cy,cz,vx,vy,vz,vp,vt,vr] = [foc,p+vp,t+vt,r+vr,cx+vx,cy+vy,cz+vz,vx,vy,vz,vp,vt,vr]
          cs = diagl (0.0001: systemNoise)
          h pars = post $ ht (syntheticCamera ( list2cam . take 7 $  pars)) (map (++[0]) world)
          restart = map (vector . (++ [0,0,0,0,0,0]) .cam2list . snd) . getPolygons Nothing world
          user (State s c p) = (list2cam $ take 7 $ toList s, s, c)

poseTrackerGen (measure,post,cz) (Just foc) world = generalTracker st0 cov0 restart measure f cs h cz user
    where st0 = vector $ (drop 1 $ cam2list $ easyCamera 40 (0,0,5) (0,0,0) 0) ++ [0,0,0,0,0,0]
          cov0 = diagl [1,1,1,10,10,10,10,10,10,5,5,5]
          f [p,t,r,cx,cy,cz,vx,vy,vz,vp,vt,vr] = [p+vp,t+vt,r+vr,cx+vx,cy+vy,cz+vz,vx,vy,vz,vp,vt,vr]
          cs = diagl systemNoise
          h pars = post $ ht (syntheticCamera ( list2cam . (foc:) .take 6 $  pars)) (map (++[0]) world)
          restart = map (vector . (++ [0,0,0,0,0,0]) . tail . cam2list . snd) . getPolygons (Just foc) world
          user (State s c p) = (list2cam $ (foc:) $ take 6 $ toList s, s, c)

-----------------------------------------------------------------------------------

withSegments world = (measure,post,cz) where
    measure img zprev = map (vector. concat . map pl . fst) . getPolygons Nothing world $ img
    post = concat
    cz = 1E-5 .* ident (2*length world)

-----------------------------------------------------------------------------------

withRegion world = (measure,post,cz) where
    measure img zprev =  map (vector.post.map pl). givemecont zprev $ img
    post = concat . map c2l . flip map [-3..3] . normalizeStart . fourierPL . Closed . map lp
    cz = 1E-5 .* diagl [1,1,1,1,10,10,50,50,10,10,1,1,1,1]


normalizeStart f = shiftStart (-t) f
    where t = phase ((f (1)- (conjugate $ f(-1))))
          shiftStart r f = \w -> cis (fromIntegral w*r) * f w

givemecont' zprev img = cs where
        Size h w = size img
        area = 5
        pixarea = h*w*area`div`1000
        rawconts = contours 10 pixarea 128 False img
        fracpix = 2
        proc = pixelsToPoints (size img).douglasPeuckerClosed fracpix.fst3
        cs = map proc $ rawconts

givemecont zprev img = cs where
        x = clip 0.95 $ zprev @> 6
        y = clip 0.65 $ zprev @> 7
        clip d x = max (-d) (min d x)
        [start] = pointsToPixels (size img) [Point x y]
        rawconts = case contourAt 5 img start of
                        Nothing -> []
                        Just p  -> [p]
        fracpix = 2
        proc = pixelsToPoints (size img) .douglasPeuckerClosed fracpix
        cs = map proc . filter ((>50).length) $ rawconts

---------------------------------------------------------------------------

--generalTracker ::
generalTracker st0 cov0 restart measure f cs h cz user = do
    let initstate = State st0 cov0 (wl h st0)
    r <- newIORef initstate
    rlost <- newIORef True
    --covz <- newIORef (ident (rows cz))
    recover <- newIORef 0
    withoutObs <- newIORef 0
    let sys = System f h cs cz
    return $ \img -> do
        st@(State _ c zprev) <- get r
        lost <- get rlost
        reco <- get recover
        woob <- get withoutObs
        let delta = if reco > 0 then 1E10 else 3
            zs = measure img zprev
            dist a b = k * sqrt ((a - b) <> icz <.> (a - b))
                where icz = inv cz
                      k = recip $ fromIntegral (rows cz)
            hasObs = not (null zs)
            z = minimumBy (compare `on` dist zprev) zs
            nsts = restart img
            hasInit = not (null nsts)
            st' = if lost
                then case hasInit of
                        False -> initstate
                        True  -> initstate {sX = head nsts, nZ = wl h (head nsts)}
                else case hasObs of
                        False -> blindUKF sys st
                        True  -> ukf sys st z
            err = dist z zprev
            obs' = if hasObs then Just (z, err) else Nothing
        r $= st'
        recover $= reco - 1
        when (lost && hasInit) (rlost $= False)
        when (hasObs && err > delta || woob > 20) $ do
            rlost $= True
            recover $= 10
        when hasObs (withoutObs $= 0)
        when (not hasObs) (withoutObs $~ (+1))
        --    covz `modifyIORef` \c -> 0.95*c + 0.05 .* ((z-zprev) `outer` (z-zprev))
        --c <- get covz
        --disp $ negate $ log10 $ fromRows [takeDiag c]
        return (user st', obs')

----------------------------------------------------------------------------

poseTrackerMonitor tracker winname mbf ref cam = do
    w3D <- evWindow3D () "UKF pose tracker" 500 (const $ kbdQuit)
    cam' <- tracker "" mbf ref cam
    return $ do
        (img,pose,(st,cov),obs) <- cam'

        inWin w3D $ do

            let scale = 0.3
                h = f (syntheticCamera pose) where f =  fromColumns . g. toColumns where g [a,b,c,d] = [a,b,d] 
                floor = warp 0 (Size 256 256) (scaling scale <> inv h) (float $ gray img)
            when (rank h == 3) $ drawTexture floor $ map (++[-0.01]) $ ht (scaling (1/scale)) [[1,1],[-1,1],[-1,-1],[1,-1]]

            setColor 1 0 0
            lineWidth $= 2
            renderPrimitive LineLoop (mapM_ vertex ref)

            lineWidth $= 1
            setColor 0 0 1
            drawCamera 0.4 (syntheticCamera pose) (Just (extractSquare 128 (float $ gray img)))

            pixelCoordinates (Size 500 500)
            setColor 0 0.5 0.5
            text2D 20 20 (shPose pose)

        return (img,pose,(st,cov),obs)

------------------------------------------------------------------------------

c2l (x:+y) = [x,y]
pl (Point x y) = [x,y]
lp [x,y] = Point x y
cam2list (CamPar f p t r (x, y, z)) = [ f, p, t, r, x, y, z ]
list2cam [ f, p, t, r, x, y, z ] = (CamPar f p t r (x, y, z))
extractCam = list2cam . take 7 . toList
fst3 (a,_,_) = a
wl f = fromList . f . toList

shPose (CamPar f p t r (cx,cy,cz)) = printf "f=%.2f  pan=%.1f  tilt=%.1f  roll=%.1f (%.1f, %.1f, %.1f)" 
                                            f (p*degree) (t*degree) (r*degree) (cx*cm) (cy*cm) (cz*cm)
    where degree = 180/pi
          cm = 10

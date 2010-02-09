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

module EasyVision.MiniApps.PoseTracker (
    poseTracker
)where


import Graphics.UI.GLUT as GL hiding (Size,Point,Matrix,matrix)
import EasyVision.GUI hiding (State)
import ImagProc hiding ((.*))
import Features
import qualified ImagProc as IP
import Control.Monad(when)
import Data.List(sort,nub,sortBy,minimumBy)
import Numeric.LinearAlgebra
import Data.IORef
import Vision
import EasyVision.MiniApps.Combinators(findPolygons,getPolygons,polyconsis)
import Util.Kalman
import Text.Printf
import Util.Stat
import Data.Array
import Debug.Trace
import ImagProc.Ipp.Core(intersection,inROI,union)
import Data.Function(on)

vector l = fromList l :: Vector Double
diagl = diag . vector
disp x = putStrLn . format " " (printf "%.0f") $ x
log10 x = log x / log 10

ukfDef = ukf ukfDefaultParam

--------------------------------------- pose tracker ---------------------------------

-- Unscented Kalman filter for pose tracking from a planar reference
poseTracker :: String -> Maybe Double -> [[Double]] -> IO Channels
            -> IO (IO(Channels, CameraParameters, (Vector Double, Matrix Double), Maybe (Vector Double, Double)))

poseTracker "" mbf ref cam = do
    tracker <- poseTrackerGen (withRegion 2 ref) mbf ref
    return $ do
        img <- cam
        ((pose,st,cov),obs) <- tracker (gray img)
        return (img, pose, (st,cov), obs)

poseTracker winname mbf ref cam = poseTrackerMonitor poseTracker winname mbf ref cam


---------------------------------------------------------------------------------

systemNoise = map (/1000) [0,0,0,0,0,0,0.01,0.01,0.01,0.1,0.1,0.1]

poseTrackerGen (measure,post,cz,restart) Nothing world = generalTracker st0 cov0 restart' measure f f2 cs h cz user
    where st0 = vector $ (cam2list $ easyCamera 40 (0,0,5) (0,0,0) 0) ++ [0,0,0,0,0,0]
          cov0 = diagl [3,1,1,1,10,10,10,10,10,10,5,5,5]
          f [foc,p,t,r,cx,cy,cz,vp,vt,vr,vx,vy,vz] = [foc,p+vp,t+vt,r+vr,cx+vx,cy+vy,cz+vz,vp,vt,vr,vx,vy,vz]
          f2 [foc,p,t,r,cx,cy,cz,vp,vt,vr,vx,vy,vz] = [foc,p,t,r,cx,cy,cz,0,0,0,0,0,0]
          cs = diagl (0.0001: systemNoise)
          h pars = post $ ht (syntheticCamera ( list2cam . take 7 $  pars)) (map (++[0]) world)
          restart' = map (vector . (++ [0,0,0,0,0,0]) .cam2list . snd) . restart Nothing world
          user (State s c p) = (list2cam $ take 7 $ toList s, s, c)

poseTrackerGen (measure,post,cz,restart) (Just foc) world = generalTracker st0 cov0 restart' measure f f2 cs h cz user
    where st0 = vector $ (drop 1 $ cam2list $ easyCamera 40 (0,0,5) (0,0,0) 0) ++ [0,0,0,0,0,0]
          cov0 = diagl [1,1,1,10,10,10,10,10,10,5,5,5]
          f [p,t,r,cx,cy,cz,vp,vt,vr,vx,vy,vz] = [p+vp,t+vt,r+vr,cx+vx,cy+vy,cz+vz,vp,vt,vr,vx,vy,vz]
          f2 [p,t,r,cx,cy,cz,vp,vt,vr,vx,vy,vz] = [p,t,r,cx,cy,cz,0,0,0,0,0,0]
          cs = diagl systemNoise
          h pars = post $ ht (syntheticCamera ( list2cam . (foc:) .take 6 $  pars)) (map (++[0]) world)
          restart' = map (vector . (++ [0,0,0,0,0,0]) . tail . cam2list . snd) . restart (Just foc) world
          user (State s c p) = (list2cam $ (foc:) $ take 6 $ toList s, s, c)

-----------------------------------------------------------------------------------

improvePoint orig rad p@(Pixel r c) = (v,best) where
    roig = roiFromPoint rad p
    h = hessian . gradients . gaussS 1.0 $ img
    img = float $ modifyROI (intersection roig) orig
    (v,Pixel r' c') = maxIndx ((-1) IP..* h)
    best = Pixel (r'+r1) (c'+c1)
    (ROI r1 _ c1 _) = theROI h

roiFromPoint rad (Pixel r c) = ROI (r-rad) (r+rad) (c-rad)  (c+rad)

improve img zprev = if ok then [imp] else []
    where pixs = pointsToPixels (size img) . map lp . toLists $  reshape 2 $ zprev
          ok = all (inROI (theROI img)) (pixs)
          imp = pixelsToPoints (size img) . map (snd.improvePoint img 15) $ pixs

withImproved world = (measure,post,cz,restart) where
    measure img zprev = impr
                        --if zprev@>0 > 0 then segs else impr
        where --segs = map (vector. concat . map pl . fst) . getPolygons Nothing world $ img
              impr = map (vector. concat . map pl. fst) $ polyconsis Nothing 0.6 world $ improve img zprev
    post = concat
    cz = 1E-5 .* ident (2*length world)
    restart = givemeconts


-----------------------------------------------------------------------------------

withSegments world = (measure,post,cz,restart) where
    measure img zprev = map (vector. concat . map pl . fst) . getPolygons Nothing world $ img -- modifyROI (intersection search) img
        where search = foldl1 union (map (roiFromPoint 20) pixs)
              pixs = pointsToPixels (size img) . map lp . toLists . reshape 2 $ zprev
    post = concat
    cz = 1E-5 .* ident (2*length world)
    restart = givemeconts -- getPolygons

-----------------------------------------------------------------------------------

withRegion w world = (measure,post,cz,restart) where
    measure img zprev =  map (vector.post.map pl). givemecont zprev $ img
    post = concat . map c2l . flip map [-w..w] . memo w. normalizeStart . fourierPL . Closed . map lp
    --cz = 1E-5 .* diagl [1,1,1,1,10,10,50,50,10,10,1,1,1,1]
    cz = 1E-5 .* ident ((2*w+1)*2)
    restart = givemeconts

normalizeStart f = shiftStart (-t) f
    where t = phase ((f (1)- (conjugate $ f(-1))))
          shiftStart r f = \w -> cis (fromIntegral w*r) * f w

givemeconts mbf ref img = cs where
        Size h w = size img
        area = 5
        pixarea = h*w*area`div`1000
        rawconts = contours 10 pixarea 128 False img ++
                   contours 10 pixarea (128+64) False img ++
                   contours 10 pixarea 64 False img
        fracpix = 5
        proc = pixelsToPoints (size img) . douglasPeucker fracpix . rot . rot. douglasPeucker fracpix.fst3
        cs = polyconsis mbf 0.4 ref $ map proc $ rawconts
        rot l = tail l ++ [head l]

givemecont zprev img = cs where
        x = clip 0.95 $ zprev @> k
        y = clip 0.65 $ zprev @> (k+1)
        k = (dim zprev -2) `div` 2
        clip d x = max (-d) (min d x)
        [start] = pointsToPixels (size img) [Point x y]
        rawconts = case contourAt 3 img start of
                        Nothing -> []
                        Just p  -> [p]
        fracpix = 2
        proc = pixelsToPoints (size img) .douglasPeuckerClosed fracpix
        cs = map proc . filter ((>50).length) $ rawconts

---------------------------------------------------------------------------

--generalTracker ::
generalTracker st0 cov0 restart measure f f2 cs h cz user = do
    let initstate = State st0 cov0 (wl h st0)
    r <- newIORef initstate
    rlost <- newIORef True
    covz <- newIORef cz
    recover <- newIORef 10
    withoutObs <- newIORef 0
    let sys1 =  System f h cs cz
        sys2 = System f2 h cs cz
    return $ \img -> do
        st@(State _ c zprev) <- get r
        lost <- get rlost
        reco <- get recover
        woob <- get withoutObs
        actcz <- get covz
        let sys = if reco > 0 then sys2 else sys1
        let delta = if reco > 0 then 1E10 else 3
            zs = measure img zprev
            dist a b = k * sqrt ((a - b) <> icz <.> (a - b))
                where icz = inv cz
                      k = recip $ fromIntegral (rows cz)
            hasObs = not (null zs)
            z = minimumBy (compare `on` dist zprev) zs
            nsts = restart img
            err = if lost then 0 else dist z zprev
            hasInit = not (null nsts)
            newlost = lost && not hasInit || woob > 10 || not lost && hasObs && err > delta
            newrecover = if newlost then 10 else reco - 1
            newwoob = if hasObs then 0 else woob +1
            st' = if lost
                then case hasInit of
                        False -> initstate
                        True  -> --trace "R" $ ukf sys initstate {sX = head nsts} (wl h (head nsts))
                                 initstate {sX = head nsts, nZ = wl h (head nsts)}
                else case hasObs of
                        False -> ukfDef sys st Nothing
                        True  -> ukfDef sys st (Just z)

            obs' = if hasObs && reco < 0 then Just (z, err) else Nothing
        r $= st'
        recover $= newrecover
        rlost $= newlost
        withoutObs $= newwoob
        --when (reco < 0 && hasObs) $ covz `modifyIORef` \c -> 0.95*c + 0.05 * ((z-zprev) `outer` (z-zprev))
        --c <- get covz
        --disp $ negate $ log10 $ fromRows [takeDiag c]
        return (user st', obs')

----------------------------------------------------------------------------

poseTrackerMonitor tracker winname mbf ref cam = do
    w3D <- evWindow3D () "UKF pose tracker" 500 (const $ kbdQuit)
    clearColor $= Color4 1 1 1 1
    --lineSmooth $= Enabled
    cam' <- tracker "" mbf ref cam
    return $ do
        (img,pose,(st,cov),obs) <- cam'

        inWin w3D $ do

            let scale = 0.3
                h = f (syntheticCamera pose) where f =  fromColumns . g. toColumns where g [a,b,c,d] = [a,b,d] 
                floor = warp 1 (Size 256 256) (scaling scale <> inv h) (float $ gray img)
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

memo t f = g where
    m = listArray (-t,t::Int) [f k | k <- [-t..t]]
    g w = m ! w

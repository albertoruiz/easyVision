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
    --poseTrackerRegion
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
import EasyVision.Combinators(warper)
import Kalman
import Text.Printf

vector l = fromList l :: Vector Double
diagl = diag . vector

normalizeStart f = shiftStart (-t) f
    where t = phase ((f (1)- (conjugate $ f(-1))))

shiftStart r f = \w -> cis (fromIntegral w*r) * f w


systemNoise = map (/1000) [0,0,0,0,0,0,0.1,0.1,0.1,0.01,0.01,0.01]

obsnoise world = ((2/640) .* ident (2*length world))
--obsnoise _ = diag . (* 1E-5) . vector $ [15,15,10,10,5,5,1,1,5,5,10,10,15,15]
--obsnoise _ = diag . (* 1E-5) . vector $ [6,6,3,3,1,1,3,3,6,6]

hpost = id
--hpost = map c2l . flip map [-3..3] . normalizeStart . fourierPL . Closed . map lp
--hpost = map c2l . flip map [-2..2] . normalizeStart . fourierPL . Closed . map lp

--rotateRight (a:as) = as ++ [a]
rotateRight = id

poseDyn world = System syspose (obspose world) (diagl (0.0001: systemNoise)) (obsnoise world)
    where syspose [f,p,t,r,cx,cy,cz,vx,vy,vz,vp,vt,vr] = [f,p+vp,t+vt,r+vr,cx+vx,cy+vy,cz+vz,vx,vy,vz,vp,vt,vr]
          obspose world pars = concat $ hpost $ ht (syntheticCamera ( list2cam . take 7 $  pars)) (map (++[0]) world)

poseDynWithF f world = System syspose (obspose world) (diagl systemNoise) (obsnoise world)
    where syspose [p,t,r,cx,cy,cz,vx,vy,vz,vp,vt,vr] = [p+vp,t+vt,r+vr,cx+vx,cy+vy,cz+vz,vx,vy,vz,vp,vt,vr]
          obspose world pars = concat $ hpost $ ht (syntheticCamera ( list2cam . (f:) . take 6 $  pars)) (map (++[0]) world)

-- Unscented Kalman filter for pose tracking from a planar reference
poseTracker :: String -> Maybe Double -> [[Double]] -> IO(Channels,[([Point],CameraParameters)]) 
            -> IO (IO(Channels, CameraParameters, (Vector Double, Matrix Double), Maybe ([Point],CameraParameters)))
poseTracker "" mbf ref cam = do
    let sys = case mbf of
                Nothing -> poseDyn ref
                Just f -> poseDynWithF f ref
        initst = case mbf of
                   Nothing -> State (vector [2,0,0,0,0,0,0,0,0,0,0,0,0]) (diagl [3,1,1,1,10,10,10,10,10,10,5,5,5]) (vector $ concat $ hpost ref)
                   Just f -> State (vector [0,0,0,0,0,0,0,0,0,0,0,0]) (diagl [1,1,1,10,10,10,10,10,10,5,5,5]) (vector $ concat $ hpost ref)
    r <- newIORef (False, initst)
    return $ do
        (orig,polys) <- cam
        (ok,st@(State p c zprev)) <- get r
        let haveObs = not . null $ polys
            g = vector . concat . map pl . fst
            f x = pnorm PNorm2 (zprev - g x)
            (obspoly,obsps) = head polys -- minimumBy (compare `on` f) polys
            obs = vector $ concat $ hpost $ map pl $ rotateRight $ obspoly
            obscam = case mbf of
                        Nothing -> vector $ (cam2list $ obsps) ++ [0,0,0,0,0,0]
                        _       -> vector $ (drop 1 $ cam2list $ obsps) ++ [0,0,0,0,0,0]

            (ok', State st' err z) = case (ok, haveObs) of
                (False,False) -> (False, st)
                (False,True)  -> (True, State obscam c obs)
                (True,False)  -> (True, blindUKF sys st)
                (True,True)   -> (True, ukf sys st obs)

        r $= (ok', State st' err z)
        let obs' = if haveObs then Just (obspoly, obsps) else Nothing
            usercam = case mbf of
                        Nothing -> extractCam st'
                        Just f  ->  list2cam . (f:) $ take 6 $ toList $ st'
        return (orig, usercam, (st', err), obs')


poseTracker winname mbf ref cam = do
    w3D <- evWindow3D () "UKF pose tracker" 500 (const $ kbdQuit)
    cam' <- poseTracker "" mbf ref cam
    return $ do
        (img,pose,(st,cov),obs) <- cam'

        inWin w3D $ do

            let scale = 0.5
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



c2l (x:+y) = [x,y]
pl (Point x y) = [x,y]
lp [x,y] = Point x y
cam2list (CamPar f p t r (x, y, z)) = [ f, p, t, r, x, y, z ]
list2cam [ f, p, t, r, x, y, z ] = (CamPar f p t r (x, y, z))
extractCam = list2cam . take 7 . toList

shPose (CamPar f p t r (cx,cy,cz)) = printf "f=%.2f  pan=%.1f  tilt=%.1f  roll=%.1f (%.1f, %.1f, %.1f)" 
                                            f (p*degree) (t*degree) (r*degree) (cx*cm) (cy*cm) (cz*cm)
    where degree = 180/pi
          cm = 10

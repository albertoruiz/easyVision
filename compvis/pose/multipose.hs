-- experiments on multiview calibration from stabilized pose

import EasyVision hiding ((.*))
import Graphics.UI.GLUT hiding (Matrix, Size, Point,set)
import Vision
import Numeric.LinearAlgebra
import Control.Monad(when)
import Quaternion
import Data.List((\\),sortBy,unzip4)
import Debug.Trace
import Text.Printf
import Kalman

main = do
    prepare

    sz <- findSize
    n <- numCams

    mbf <- maybeOption "--focal"

    let ref = cornerRef
        vc c = c >>= withChannels >>= poseTracker "" mbf ref >>= inThread
    cams <- mapM (vc . flip getCam sz) [0..n-1]


    let initState = replicate n cameraAtOrigin

    --w <- evWindow 0 "multipose" sz Nothing (mouse $ kbdQuit)
    w3D <- evWindow3D () "3D view" 400 (const $ kbdQuit)
    clearColor $= Color4 1 1 1 1
    lineSmooth $= Enabled
    w3DSt <- evWindow3D initState "Camera Reference" 400 (toSave $ kbdQuit)
    clearColor $= Color4 1 1 1 1
    lineSmooth $= Enabled
    wm <- evWindow () "views" (Size 150 (200*n)) Nothing (const $ kbdQuit)

    launchFreq 25 $ do
        (imgs,eps,sts,mbobs) <- unzip4 `fmap` sequence cams
        let fig v = toLists $ reshape 2 v
            rects = map (map (fig.fst).maybeToList) mbobs
            --ps    = map (map (syntheticCamera.snd).maybeToList) mbobs
            f _    _ Nothing = []
            f cond p (Just (_,err)) = if cond err then [syntheticCamera p] else []
            ps    = zipWith (f (<0.2)) eps mbobs
            other = zipWith (f (>=0.2)) eps mbobs
{-
        inWin w $ do -- monitorization window
            c' <- getW w
            let c = c' `mod` n
                img = imgs!!c
                rect = rects!!c

            drawImage (gray img)
            pointCoordinates (size (gray img))

            setColor 1 0 0
            lineWidth $= 3
            mapM_ (renderPrimitive LineLoop . (mapM_ vertex)) rect
            text2D 0.9 0.6 (show (length rect))
-}
        inWin wm $ drawImage $ blockImage [map gray imgs]


        inWin w3D $ do -- reference world
            setColor 0 0 0
            lineWidth $= 1
            renderPrimitive LineLoop (mapM_ vertex ref)

            lineWidth $= 1
            setColor 0 0 1
            mapM_ (\c -> drawCamera 0.4 c Nothing) (concat other)
            lineWidth $= 2
            setColor 0 0 1
            mapM_ (\c -> drawCamera 0.4 c Nothing) (concat ps)

        --print $ prepareObs ps

        st' <- getW w3DSt
        let st = update st' ps
        putW w3DSt st

        --print st

        inWin w3DSt $ do -- camera reference
            setColor 1 0 0
            lineWidth $= 1
            mapM_ (\c -> drawCamera 0.4 c Nothing) st



update st ps = r where
    r = if null obs then st else st'
    obs = prepareObs ps
    b = fst.fst.head $ obs
    rb = (fst.toCameraSystem) (st!!b)
    stb = map (<>rb) st
    rest = map (\x->(x,Nothing)) (pairs \\ map fst obs)
    pairs = [(b,k)| k <- [0..length st -1], k/= b]
    camb = kgen fb <> cameraAtOrigin
    fb = snd $ toCameraSystem $ ps!!b!!0
    fullobs = map snd $ sortBy (compare `on` fst) (obs++rest++ [((b,b),Just camb)])
    stb' = zipWith f stb fullobs
    st'0 = (fst.toCameraSystem) (stb'!!0)
    st' = map (<>st'0) stb'
    f s Nothing = s
    f s (Just x) = asMatrix $ weighted 0.95 (betterRep s) (betterRep x)

prepareObs ps = proc obs where
    obs = filter (not.null.snd) $ zip [0..] (map (take 1) ps)
    proc [] = []
    proc ((k,[p]):rest) = map f rest
        where f (q,[t]) = ((k,q), Just (t<>r))
                  where r = fst . toCameraSystem $ p

------------------------------------------------

betterRep = withQuat . factorizeCamera
    where withQuat (k,r,c) = (k@@>(0,0), toQuat r, c)
          toQuat = uncurry axisToQuat . rotToAxis

asMatrix (f,q,c) = (kgen f) <> (r <|> -r <> c)
    where r = getRotation $ q

weighted alpha (f1,r1,c1) (f2,r2,c2) = (alpha*f1+(1-alpha)*f2,
                                        slerp r1 r2 alpha,
                                        alpha.*c1+(1-alpha).*c2)

------------------------------------------------

mouse _ st (MouseButton WheelUp) Down _ _ = do
    st $~ (+1)
mouse _ st (MouseButton WheelDown) Down _ _ = do
    st $~ (subtract 1)
mouse def _ a b c d = def a b c d

maybeToList Nothing = []
maybeToList (Just a)  = [a]

toSave _ st (Char 's') Down _ _ = do
    cams <- get st
    writeFile "cameras.txt" (show cams)
toSave def _ a b c d = def a b c d
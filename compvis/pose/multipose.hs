-- experiments on multiview calibration from stabilized pose

import EasyVision hiding ((.*))
import Graphics.UI.GLUT hiding (Matrix, Size, Point,set,triangulate)
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
    mbfs <- maybeOption "--focals"

    let focs = case mbfs of
                Just l -> map Just l
                Nothing -> repeat mbf

    print (take n focs)

    let ref = cornerRef
        vc c mbf = c >>= withChannels >>= poseTracker "" mbf ref >>= inThread
    cams <- sequence $ zipWith vc (map (flip getCam sz) [0..n-1]) focs


    let initState = replicate n cameraAtOrigin

    --w <- evWindow 0 "multipose" sz Nothing (mouse $ kbdQuit)
    w3D <- evWindow3D () "World Reference" 400 (const $ kbdQuit)
    clearColor $= Color4 1 1 1 1
    --lineSmooth $= Enabled
    w3DSt <- evWindow3D initState "Camera Reference" 600 (toSave $ kbdQuit)
    clearColor $= Color4 1 1 1 1
    lineSmooth $= Enabled
    wm <- evWindow () "views" (Size 150 (200*n)) Nothing (const $ kbdQuit)

    launchFreq 30 $ do
        (imgs,eps,sts,mbobs) <- unzip4 `fmap` sequence cams
        let fig v = toLists $ reshape 2 v
            rects = map (map (fig.fst).maybeToList) mbobs
            --ps    = map (map (syntheticCamera.snd).maybeToList) mbobs
            f _    _ Nothing = []
            f cond p (Just (z,err)) = if cond err then [(syntheticCamera p, toLists $ reshape 2 z)] else []
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
            lineWidth $= 2
            renderPrimitive LineLoop (mapM_ vertex ref)

            lineWidth $= 1
            setColor 0 0 1
            mapM_ (\c -> drawCamera 0.4 c Nothing) (concatMap (map fst) other)
            lineWidth $= 2
            setColor 0 0 1
            mapM_ (\c -> drawCamera 0.4 c Nothing) (concatMap (map fst) ps)

        --print $ prepareObs ps

        st' <- getW w3DSt
        let st = update st' (map (map fst) ps)
        putW w3DSt st

        --print st

        inWin w3DSt $ do -- camera reference
            setColor 0 0 0
            lineWidth $= 2
            let g c im = drawCamera 0.5 c (Just $ extractSquare 128 $ float $ gray im)
            sequence $ zipWith g st imgs
            let f   _ Nothing = []
                f p (Just (z,err)) = [(p, toLists $ reshape 2 z)]
                allofthem = concat $ zipWith f st mbobs
            when (False && length allofthem > 1) $ do
               let pts3D = triangulate allofthem
               lineWidth $= 2
               setColor 1 0 0
               lineWidth $= 1
               pointSize $= 5
               renderPrimitive LineLoop $ mapM_ vertex pts3D
               renderPrimitive Points $ mapM_ vertex pts3D
               pixelCoordinates (Size 600 600)
               setColor 0 0 0
               text2D 20 20 (printf "Maximum Error = %.0f mm" $ 10 * pnorm Infinity (distMat pts3D - distMat ref))
               text2D 20 40 (printf "Object Size = %.1f cm" $ dis (pts3D!!0) (pts3D!!1))
               text2D 20 60 (printf "Distance to C0 = %.1f cm" $ dis [0,0,0] (pts3D!!0))



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

------------------------------------------------

distMat pol = fromList [ dis p1 p2 | p1 <- pol, p2 <- pol]

dis l1 l2 = (*cm).  sqrt . sum . map (^2) $ zipWith (-) l1 l2

cm = 10


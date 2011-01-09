-- experiments on multiview calibration from stabilized pose
-- ./multipose swebcam0 swebcam1 webcam2 '--focals=[1.7,1.7,2.7]' +RTS -N2 -RTS --alpha=0.95

import EasyVision hiding ((.*),State)
import qualified EasyVision as EV
import Graphics.UI.GLUT hiding (Matrix, Size, Point,set,triangulate,scale)
import Vision
import Numeric.LinearAlgebra
import Control.Monad(when,(>=>))
import Util.Quaternion
import Data.List((\\),sortBy,unzip4)
import Debug.Trace
import Text.Printf
import Util.Kalman
import Data.IORef
import Data.Maybe

main = do
    prepare

    sz <- findSize
    n <- numCams

    mbf <- maybeOption "--focal"
    mbfs <- maybeOption "--focals"

    alpha <- getOption "--alpha" 0.95

    let focs = case mbfs of
                Just l -> map Just l
                Nothing -> repeat mbf

    print (take n focs)

    let ref = cornerRef
        vc c mbf = c >>= poseTracker "" mbf ref -- >>= inThread

    rawcams <- mapM (flip getCam sz >~> channels) [0..n-1]
    cams <- sequence $ zipWith vc (map return rawcams) focs


    let initState = replicate n (kgen 2 <> cameraAtOrigin)

    w3D <- evWindow3D () "World Reference" 400 (const $ kbdQuit)
    clearColor $= Color4 1 1 1 1
    --lineSmooth $= Enabled
    w3DSt <- evWindow3D initState "Camera Reference" 600 (toSave $ kbdQuit)
    clearColor $= Color4 1 1 1 1
    lineSmooth $= Enabled
    wm <- evWindow False "views" (Size 150 (200*n)) Nothing (toLock $ kbdQuit)

    auxWin <- evWindow 0 "repro" sz Nothing (chView $ kbdQuit)

    plstate <- newIORef undefined
    pfs <- newIORef undefined

    launch $ do
      lock <- getW wm
      if lock
       then do
         nv <- getW auxWin
         imgs <- fmap (map gray) (sequence rawcams)
         inWin wm $ drawImage $ blockImage [imgs]
         s@(State st _ _) <- get plstate
         let zpre = observPrev st
             zobs = findFeatures zpre imgs
             h = hFun st imgs
             ns = dim st
             nz = dim (h st)
             sys = System fFun (toList.h.fromList) (1E-5 * ident ns) (1E-5 * ident nz)
             s' = ukf ukfDefaultParam sys s (Just $ fromList $ concat $ map pl $ catMaybes $ concat zobs)
             --s' = s
         --print (dim $ h st, dim st)
         --print $ concat zobs
         plstate $= s'
         inWin auxWin $ do
            drawImage (imgs!! mod nv (length imgs))
            pointCoordinates sz
            pointSize $= 5
            setColor 1 0 0
            renderPrimitive Points $ mapM_ vertex (zpre!! mod nv (length zpre))
            pointSize $= 2
            setColor 1 1 0
            renderPrimitive Points $ mapM_ vertex (concatMap maybeToList $ zobs!! mod nv (length zpre))

         let st' = st
         inWin w3DSt $ do -- camera reference
            let c0:cs = map (asMatrix.paramsvec) $ tail $ toRows $ reshape 8 st'
                st = kgen (st'@>8) <> cameraAtOrigin : cs
            setColor 0 0 0
            lineWidth $= 2
            let g c im = drawCamera 0.1 c (Just $ extractSquare 128 $ float im)
            sequence $ zipWith g st imgs
            print $ head $ toColumns $ reshape 8 st'

            let axs = inv $ fst $ toCameraSystem c0
                f pose = (pose, ht (pose<>axs) (map (++[0]) ref))
                predicted = f (c0<>inv axs): tail (map f st)
                g img (p, pts) = (p, improve 10 (img) pts)
                allofthem = zipWith g imgs predicted
            when (length allofthem > 1 && all (not.null.snd) allofthem) $ do
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
               let d k = printf "%.1f  " $ dis [0,0,0] [cx,cy,cz]
                    where (cx,cy,cz) = cameraCenter.poseFromFactorization.factorizeCamera $ (st!!k)
               text2D 20 80 $ "Distance between cameras (cm) =" ++ concatMap d [1..length st -1]



       else do
        (imgs,eps,sts,mbobs) <- unzip4 `fmap` sequence cams
        let fig v = toLists $ reshape 2 v
            rects = map (map (fig.fst).maybeToList) mbobs
            --ps    = map (map (syntheticCamera.snd).maybeToList) mbobs
            f _    _ Nothing = []
            f cond p (Just (z,err)) = if cond err then [(syntheticCamera p, toLists $ reshape 2 z)] else []
            ps    = zipWith (f (<0.2)) eps mbobs
            other = zipWith (f (>=0.2)) eps mbobs

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
        let st = update alpha st' (map (map fst) ps)
        putW w3DSt st

        let lst = lockedState (head eps) st
        plstate $= State lst (1E-2 * ident (dim lst)) undefined
        pfs $= map focalDist eps

        inWin w3DSt $ do -- camera reference
            setColor 0 0 0
            lineWidth $= 2
            let g c im = drawCamera 0.5 c (Just $ extractSquare 128 $ float $ gray im)
            sequence $ zipWith g st imgs

            let axs = inv $ fst $ toCameraSystem (syntheticCamera (eps!!0))
                f pose = (pose, ht (pose<>axs) (map (++[0]) ref))
                predicted = map f st
                g img (p, pts) = (p, improve 10 (gray img) pts)
                allofthem = zipWith g imgs predicted
            when (length allofthem > 1 && all (not.null.snd) allofthem) $ do
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
               let d k = printf "%.1f  " $ dis [0,0,0] [cx,cy,cz]
                    where (cx,cy,cz) = cameraCenter.poseFromFactorization.factorizeCamera $ (st!!k)
               text2D 20 80 $ "Distance between cameras (cm) =" ++ concatMap d [1..length st -1]



update alpha st ps = r where
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
    f s (Just x) = asMatrix $ weighted alpha (betterRep s) (betterRep x)

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

asMatrix (f,q,c) = (kgen f) <> (r & asColumn (-r <> c))
    where r = quatToRot q

weighted alpha (f1,r1,c1) (f2,r2,c2) = (alpha*f1+(1-alpha)*f2,
                                        slerp r2 r1 alpha,
                                        alpha`scale`c1+(1-alpha)`scale`c2)

------------------------------------------------

mouse _ st (MouseButton WheelUp) Down _ _ = do
    st $~ (+1)
mouse _ st (MouseButton WheelDown) Down _ _ = do
    st $~ (subtract 1)
mouse def _ a b c d = def a b c d

toSave _ st (Char 's') Down _ _ = do
    cams <- get st
    writeFile "cameras.txt" (show cams)
toSave def _ a b c d = def a b c d

toLock _ st (MouseButton LeftButton) Down _ _ = do
    st $~ not
toLock def _ a b c d = def a b c d

chView _ st (MouseButton WheelDown) Down _ _ = do
    st $~ subtract 1
chView _ st (MouseButton WheelUp) Down _ _ = do
    st $~ (+1)
chView def _ a b c d = def a b c d

-------------------------------------------------

distMat pol = fromList [ dis p1 p2 | p1 <- pol, p2 <- pol]

dis l1 l2 = (*cm).  sqrt . sum . map (^2) $ zipWith (-) l1 l2

cm = 10

a & b = fromBlocks[[a,b]]

-------------------------------------------------

lp [x,y] = Point x y
pl (Point x y) = [x,y]

improvePoint orig rad p@(Pixel r c) = (v,best) where
    roig = roiFromPoint rad p
    h = ((-1) EV..*) . hessian . gradients . gaussS' 1.5 1.0 $ img
    img = float $ modifyROI (intersection roig) orig
    (v,Pixel r' c') = maxIndx $ {-trace (show $ roiSize $ theROI h)-} h
    best = Pixel (r'+r1) (c'+c1)
    (ROI r1 _ c1 _) = theROI h

roiFromPoint rad (Pixel r c) = ROI (r-rad) (r+rad) (c-rad)  (c+rad)

improve rad img points = if ok then imp else []
    where pixs = pointsToPixels (size img) (map lp points)
          ok = all (inROI (theROI img)) (pixs)
          imp = map pl . pixelsToPoints (size img) . map (snd.improvePoint img rad) $ pixs

-----------------------------------

lockedState p0 st = join $ constant 0 8 : (g p0) : map (vecparams.betterRep) (tail st)
    where g = vecparams.betterRep.syntheticCamera

observPrev lstv = obs0:obs where
    c0:cs = map (asMatrix.paramsvec) $ tail $ toRows $ reshape 8 lstv
    axs = inv $ fst $ toCameraSystem c0
    okref = map (++[0]) (take 3 $ cornerRef) --- !!!
    obs0 = ht c0 okref
    f c = ht (c <> axs) okref
    obs  = map f cs

-- t = asMatrix.paramsvec.vecparams.betterRep

-- working with quaternions because poseFromFactorization is problematic
vecparams (f,(Quat s v),c) = join [fromList [f], fromList [s], v,c]
paramsvec v = (v@>0, (Quat (v@>1) (subVector 2 3 v)), subVector 5 3 v)

-----------------------------------

findFeatures = zipWith improveMB where
     improveMB pts img = map (improvePointMB 1 img 10) pts

improvePointMB th orig rad p' = if inROI roi p && v>th then Just best' else Nothing
    where (v,best) = improvePoint orig rad p
          roi = shrink  (20,20) (theROI orig)
          [p] = pointsToPixels (size orig) [lp p']
          [best'] = pixelsToPoints (size orig) [best]

hFun st imgs = g
    where zValid = concat $ findFeatures (observPrev st) imgs
          sel (Just _) z = Just z
          sel Nothing _  = Nothing
          g st = fromList $ concat $ catMaybes $ zipWith sel zValid (concat $ observPrev st)

fFun st = v ++ zipWith (+) c0x v ++ cs where
    (c0,cs) = splitAt 16 st
    (v,c0x) = splitAt 8 c0

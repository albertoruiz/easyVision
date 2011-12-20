-- experiments on multiview calibration from stabilized pose
-- ./multipose swebcam0 swebcam1 webcam2 '--focals=[1.7,1.7,2.7]' +RTS -N2 -RTS --alpha=0.95

import EasyVision hiding ((.*),State)
import qualified EasyVision as EV
import Graphics.UI.GLUT hiding (Matrix, Size, Point, triangulate)
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
import Control.Applicative((<$>),(<*>))
import ImagProc.Ipp.Core
import GHC.Float(float2Double)
import Util.Options

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

    auxWin <- evWindow 0 "repro" sz Nothing (chView $ kbdQuit)

    hv <- signalMonitor "Reconstruction Error" 50 100 (printf "%.2f mm") (0,10)

    w3D <- evWindow3D () "World Reference" 400 (const $ kbdQuit)
    clearColor $= Color4 1 1 1 1
    --lineSmooth $= Enabled
    w3DSt <- evWindow3D initState "Camera Reference" 600 (toSave $ kbdQuit)
    clearColor $= Color4 1 1 1 1
    lineSmooth $= Enabled
    wm <- evWindow False "views" (Size 150 (200*n)) Nothing (toLock $ kbdQuit)

    plstate <- newIORef undefined
    pfs <- newIORef undefined

    launch $ do

      lock <- getW wm
      if lock
       then do
         nv <- getW auxWin
         imgs <- fmap (map gray) (sequence rawcams)

         --print $ env3x3 (imgs!!0) (Pixel 100 100)
         --print $ subPixMean (imgs!!0) (Pixel 100 100)

         inWin wm $ drawImage $ blockImage [imgs]
         (s@(State st stc _),tfixed) <- get plstate
         --print $ (map (round . (*1000))) $ toList $ subVector 6 12 st
         fs <- get pfs
         let itf = tfixed
             zpre = observPrev itf fs st
             zobs = findFeatures zpre imgs
             h = hFun itf fs st imgs
             ns = dim st
             nz = dim (h st)
             cz = 1E-5 * ident nz
             systemNoise = map (/1000) [0.01,0.01,0.01,0.1,0.1,0.1]
             cs = diagl $ replicate 6 0 ++ concat (replicate (n-1) (map(/1000) systemNoise)) ++ systemNoise
             sys = System fFun (toList.h.fromList) cs cz
             s' = ukf ukfDefaultParam sys s (Just $ fromList $ concat $ map pl $ catMaybes $ concat zobs)
             --s' = s
         --print (dim $ h st, dim st)
         --print $ concat zobs
         plstate $= (s',tfixed)
         inWin auxWin $ do
            drawImage (imgs!! mod nv (length imgs))
            pointCoordinates sz
            pointSize $= 5
            setColor 1 0 0
            renderPrimitive Points $ mapM_ vertex (zpre!! mod nv (length zpre))
            pointSize $= 3
            setColor 0 0 1
            renderPrimitive Points $ mapM_ vertex (concatMap maybeToList $ zobs!! mod nv (length zpre))

         let c0:cs = map (syntheticCamera.list2cam) $ zipWith (:) fs (init $ toLists $ reshape 6 st)
             axs = inv $ fst $ toCameraSystem c0
             st' = kgen (head fs) <> cameraAtOrigin : map (<> tfixed) cs

         inWin w3DSt (showRec ref st' imgs axs hv)


       else do
        (imgs,eps,sts,mbobs) <- unzip4 `fmap` sequence cams
        let fig v = toLists $ reshape 2 v
            rects = map (map (fig.fst).maybeToList) mbobs
            --ps    = map (map (syntheticCamera.snd).maybeToList) mbobs
            f _    _ Nothing = []
            f cond p (Just (z,err)) = if cond err then [(syntheticCamera p, toLists $ reshape 2 z)] else []
            ps    = zipWith (f (<0.3)) eps mbobs
            other = zipWith (f (>=0.3)) eps mbobs

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

        let fs = map focalDist eps
            lst = lockedState fs sts
            tfixed = fst $ toCameraSystem $ syntheticCamera (eps!!0)
        plstate $= (lst,tfixed)
        pfs $= fs

        let axs = inv $ fst $ toCameraSystem (syntheticCamera (eps!!0))
        inWin w3DSt (showRec ref st (map gray imgs) axs hv)


-----------------------------------------------------------

showRec ref st imgs axs hv = do -- camera reference
    setColor 0 0 0
    lineWidth $= 2
    let g c im = drawCamera 0.5 c (Just $ extractSquare 128 $ float im)
    sequence $ zipWith g st imgs

    let f pose = (pose, ht (pose<>axs) (map (++[0]) ref))
        predicted = map f st
        g img (p, pts) = (p, improve 10 img pts)
        allofthem = take 2 $ zipWith g imgs predicted

    when (length allofthem > 1 && all (not.null.snd) allofthem) $ do
        let pts3D = triangulate allofthem
            est3D = ht axs (map (++[0]) ref)
        lineWidth $= 0.1
        setColor 0.5 0.5 0.5
        renderPrimitive LineLoop $ mapM_ vertex est3D
        setColor 1 0 0
        lineWidth $= 1
        pointSize $= 5
        renderPrimitive LineLoop $ mapM_ vertex pts3D
        renderPrimitive Points $ mapM_ vertex pts3D
        pixelCoordinates (Size 600 600)
        setColor 0 0 0
        --let mxerr = 10 * pnorm Infinity (distMat pts3D - distMat ref)
        let mxerr = mm * maximum (zipWith err pts3D est3D)
                where err x y = norm (fromList x - fromList y)
        text2D 20 20 (printf "Maximum Error = %.1f mm" mxerr)
        text2D 20 40 (printf "Object Size = %.1f cm" $ dis (pts3D!!0) (pts3D!!1))
        text2D 20 60 (printf "Distance to C0 = %.1f cm" $ dis [0,0,0] (pts3D!!0))
        let c k = pnorm PNorm2 $ inHomog $ nullVector (st!!k)
        let d k = printf "%.1f  " (c k*cm)
        text2D 20 80 $ "Distance between cameras (cm) =" ++ concatMap d [1..length st -1]
        hv [mxerr]




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

asMatrix (f,q,c) = kgen f <> fromBlocks [[r, asColumn (-r <> c)]]
    where r = quatToRot q

weighted alpha (f1,r1,c1) (f2,r2,c2) = (alpha*f1+(1-alpha)*f2,
                                        slerp r2 r1 alpha,
                                        scalar alpha*c1 + scalar (1-alpha) *c2)

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
mm = 10*cm

-------------------------------------------------

lp [x,y] = Point x y
pl (Point x y) = [x,y]
cam2list (CamPar f p t r (x, y, z)) = [ f, p, t, r, x, y, z ]
list2cam [ f, p, t, r, x, y, z ] = (CamPar f p t r (x, y, z))
diagl = diag . fromList
norm = pnorm PNorm2

improvePoint orig rad p@(Pixel r c) = (v,best) where
    roig = roiFromPoint rad p
    h = ((-1) EV..*) . hessian . gradients . gaussS' 1.5 1.0 $ img
    img = float $ modifyROI (intersection roig) orig
    (v,Pixel r' c') = maxIndx h
    best = subPixMean h (Pixel (r'+r1) (c'+c1))
    (ROI r1 _ c1 _) = theROI h

roiFromPoint rad (Pixel r c) = ROI (r-rad) (r+rad) (c-rad)  (c+rad)

improve rad img points = if ok then imp else []
    where pixs = pointsToPixels (size img) (map lp points)
          ok = all (inROI (theROI img)) (pixs)
          imp = map pl . map (snd.improvePoint img rad) $ pixs

improvePointMB th orig rad p' = if inROI roi p && v>th then Just best else Nothing
    where (v,best) = improvePoint orig rad p
          roi = shrink  (20,20) (theROI orig)
          [p] = pointsToPixels (size orig) [lp p']

-----------------------------------

lockedState fs sts = State (rel fs nv') nc' 1
    where n = dim (fst (head sts))
          w = length sts
          nv = join $ map (subVector 0 6 . fst) sts
          nc = fromBlocks [ [g i j | i <- [0..w-1]] | j <- [0..w-1]]
          cs = map (subMatrix (0,0) (6,6). snd) sts
          g i j | i==j      = cs!!i
                | otherwise = zero 6
          zero k = diag (constant 0 k)
          zeroR n m = reshape m (constant 0 (n*m))
          dc = diag (subVector 6 6 (takeDiag (snd $ head sts)))
          nv' = join [nv, subVector 6 6 (fst $ head sts)]
          nc' = fromBlocks [ [ nc                , zeroR (rows nc) 6]
                           , [ zeroR 6 (cols nc) , dc               ]]

rel fs v = join [subVector 0 6 v, join (map fromList cs), constant 0 6] where
    m0:ms = map (syntheticCamera.list2cam) $ zipWith (:) fs (init $ toLists $ reshape 6 $ v)
    --t = debug $ fst $ toCameraSystem m0
    t = ident 4
    cs = map (tail . cam2list . poseFromCamera . (<>t)) ms

observPrev tfixed fs lstv = obs0:obs where
    c0:cs = map list2cam $ zipWith (:) fs (init $ toLists $ reshape 6 $ lstv)
    t = inv $ fst $ toCameraSystem $ syntheticCamera c0
    okref = ht t $ map (++[0]) (take 6 $ cornerRef) --- !!!
    f c = ht (syntheticCamera c<>tfixed) okref
    obs  = map f cs
    obs0  = ht (kgen (head fs) <> cameraAtOrigin) okref


-----------------------------------

findFeatures = zipWith improveMB where
     improveMB pts img = map (improvePointMB 1 img 10) pts

hFun tfixed fs st imgs = g
    where zValid = concat $ findFeatures (observPrev tfixed fs st) imgs
          sel (Just _) z = Just z
          sel Nothing _  = Nothing
          g st = fromList $ concat $ catMaybes $ zipWith sel zValid (concat $ observPrev tfixed fs st)

fFun st = concat [zipWith (+) v x, concat cs, v]
    where  xs = splitEvery 6 st
           x = head xs
           v = last xs
           cs = init . tail $ xs


splitEvery _ [] = []
splitEvery k x = take k x : splitEvery k (drop k x)

-----------------------------------------------------------------------------

historyVals :: String        -- ^ window title
             -> Int           -- ^ history length
             -> Size          -- ^ size in pixels of the zoom window
             -> (Double -> String) -- ^ monitorization of the mean value
             -> (Double -> Double) -- ^ scaling to 0 - 1
             -> IO (Double -> IO ()) -- ^ update function
historyVals title n sz fsh fsc = do
    w <- evWindow []
                  title sz (Just disp) (const kbdQuit)
    clearColor $= Color4 1 1 1 1
    let f x = do
            vals <- getW w
            putW w (x: take (n-1) vals)
            postRedisplay (Just (evW w))
    return f
    where
    Size h w = sz
    disp st = do
        vals <- get st
        setColor 1 0 0
        pointSize $= 5
        pixelCoordinates sz
        let f x k = vertex (Pixel (h-round (fsc x)) (4*k))
        renderPrimitive LineStrip $ sequence_ $ zipWith f vals [0..]
        let m = sum vals / fromIntegral n
            pm = h - round (fsc m)
        setColor 0.4 0.4 0.4
        renderPrimitive Lines $ vertex (Pixel pm 0) >> vertex (Pixel pm w)
        setColor 0 0 0
        text2D 15 15 (fsh m)

{-    mouse _ st (MouseButton WheelUp) Down _ _ = do
        (im,p,z,ok) <- get st
        st $= clip (im,p,z+(max 1 $ z`div`10),ok)
        postRedisplay Nothing-}

---------------------------------------------------------

env3x3 img (Pixel r c) = (3><3) $ v <$> [-1,0,1] <*> [-1,0,1] :: Matrix Double
    where v i j = float2Double $ fval img (Pixel (r+i) (c+j))

subPixMean img pix@(Pixel r c) = Point x' y' where
    rs = (3><3) [-1,-1,-1,0,0,0,1,1,1]
    cs = trans rs
    env = env3x3 img pix
    sumV x = x <.> constant 1 (dim x)
    sumM = sumV . flatten
    w = env / scalar (sumM env)
    dr = sumM (rs * w)
    dc = sumM (cs * w)
    sc = 2 / fromIntegral (width (size img))
    [Point x y] = pixelsToPoints (size img) [pix]
    x' = x - dc*sc
    y' = y - dr*sc

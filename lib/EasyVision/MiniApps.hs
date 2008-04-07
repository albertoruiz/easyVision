-----------------------------------------------------------------------------
{- |
Module      :  EasyVision.MiniApss
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Useful windows with associated behaviour.

-}
-----------------------------------------------------------------------------

module EasyVision.MiniApps (
    readCatalog,
    getCatalog,
    catalogBrowser,
    hsvPalette,
    scatterWindow,
    regionDetector, regionTracker,
    panoramic,
    poseTracker
)where

import Graphics.UI.GLUT as GL hiding (Size,Point,Matrix,matrix)
import EasyVision.GUI hiding (State)
import EasyVision.Parameters
import ImagProc hiding ((.*))
import Data.List(transpose)
import Control.Monad(when)
import ImagProc.Ipp.Core
import Foreign.C.Types(CUChar)
import Foreign
import qualified Data.Map as Map
import Data.List(sort,nub,sortBy)
import EasyVision.Util
import Numeric.LinearAlgebra
import Classifier.Base(group)
import Data.IORef
import Vision
import Numeric.GSL.Minimization
import EasyVision.Combinators(warper)
import Kalman
import Text.Printf

-- | reads a labeled video
readCatalog :: String -> Size -> String -> Maybe Int -> (ImageYUV-> a) -> IO [(a,String)]
readCatalog video sz classesfile mbn prepro = do
    cam <- mplayer (video++" -benchmark") sz
    rawclasses <- readFile classesfile
    let classfilelines = lines rawclasses
        effectivelines = case mbn of
            Nothing -> classfilelines
            Just n  -> take n classfilelines
    let n = length effectivelines
        words' s = let (n:ws) = words s in [n , unwords ws]
    let [frames, classes] = transpose $ map words' effectivelines
    when (map read frames /= [1..n]) $ error ("inconsistent file "++ classesfile)
    imgs <- sequence (replicate n cam)
    return (zip (map prepro imgs) classes)

----------------------------------------------------------

classMap :: [[String]] -> String -> String
classMap lls = if null lls then id else search
    where f l@(h:_) = [(e,h)| e <- l]
          m = Map.fromList $ concatMap f lls
          search v = case Map.lookup v m of
                        Nothing -> v
                        Just c  -> c

-- | higher level version of getCatalog allowing for --group and --desired
getCatalog :: String -> Size -> String -> Maybe Int -> (ImageYUV-> a) -> IO [(a,String)]
getCatalog name sz lbs mbn feat = do
    dat <- readCatalog name sz lbs mbn feat

    group <- fmap classMap $ getOption "--group" []

    desi <- getFlag "desired"

    desired <- if not desi
                then fmap concat $ getOption "--group" []
                else getOption "--desired" []

    let okclasses = [(img, group cl) | (img,cl) <- dat, cl /= "?", desired == [] || cl `elem` desired]

    putStr "Valid images: "
    print (length okclasses)
    putStr "Classes found: "
    print (sort $ nub $ map snd okclasses)
    return okclasses


-- | to do (ImageYUV???)
catalogBrowser :: Int -> [(ImageYUV, String)] -> String -> Size -> IO (EVWindow (Int, [(ImageYUV, String)]))
catalogBrowser n catalog name sz =
    evWindow (n-1,catalog) name sz (Just disp) (mouse $ kbdQuit)
  where
    disp st = do
        (k,catalog) <- get st
        let (img,label) = catalog!!k
        drawImage img
        windowTitle $= name++" #"++show (k+1)++ ": "++label
    mouse _ st (MouseButton WheelUp) Down _ _ = do
        (k,catalog) <- get st
        st $= (min (k+1) (length catalog -1), catalog)
        postRedisplay Nothing
    mouse _ st (MouseButton WheelDown) Down _ _ = do
        (k,catalog) <- get st
        st $= (max (k-1) 0, catalog)
        postRedisplay Nothing
    mouse def _ a b c d = def a b c d

-------------------------------------------------------------------------------------

hsvPalette :: IO (EVWindow (CUChar, CUChar, CUChar))
hsvPalette = evWindow (128,128,255) "HSV" (Size 256 256) (Just disp) (mouse kbdQuit)
  where
    disp st = do
        (r',c',k) <- get st
        drawImage (palette k)
        pixelCoordinates (Size 256 256)
        setColor 0 0 0
        let r = fromIntegral r'
            c = fromIntegral c'
        renderPrimitive LineLoop $ mapM_ vertex
            [Pixel (r-2) (c-2), Pixel (r-2) (c+2), Pixel (r+2) (c+2), Pixel (r+2) (c-2)]
        text2D 20 20 (show (c,r,k))
    mouse _ st (MouseButton WheelUp) Down m _ = do
        (r,c,k) <- get st
        st $= (r,c,k + if GL.shift m == Down then 10 else 1)
        postRedisplay Nothing
    mouse _ st (MouseButton WheelDown) Down m _ = do
        (r,c,k) <- get st
        st $= (r,c,k- if GL.shift m == Down then 10 else 1)
        postRedisplay Nothing
    mouse _ st (MouseButton LeftButton) Down m (Position x y) = do
        (_,_,k) <- get st
        st $= (fromIntegral y, fromIntegral x,k)
        postRedisplay Nothing
    mouse def _ a b c d = def a b c d


palette k = hsvToRGB $ putChannels (fromLists ramp ,
                                    fromLists (transpose ramp),
                                    constImage k (Size 256 256))
    where
        ramp = replicate 256 [0..255]

        fromLists ls = unsafePerformIO $ do
            r <- image (Size 256 256)
            setData8u r ls
            return r

---------------------------------------------------------------------------

scatter examples (i,j) = do
    let (gs,lbs) = group examples
        plot = map (\v-> Point (v@>i) (v@>j))
        xs = map ((@>i).fst) examples
        ys = map ((@>j).fst) examples
        a1 = minimum xs
        a2 = maximum xs
        b1 = minimum ys
        b2 = maximum ys
        da = 0.05*(a2-a1)
        db = 0.05*(b2-b1)
        colors = [setColor 1 0 0, setColor 0 0 1, setColor 0 1 0] ++
                 [setColor 1 1 0, setColor 0 1 1, setColor 1 0 1] ++
                 [setColor 1 0.5 0.5, setColor 0.5 0.5 1, setColor 0.5 1 0.5] ++
                 repeat (setColor 1 1 1)
    clear [ColorBuffer]
    matrixMode $= Projection
    loadIdentity
    ortho2D (a1-da) (a2+da) (b1-db) (b2+db)
    matrixMode $= Modelview 0
    loadIdentity
    let f pts col = do
            col
            GL.renderPrimitive GL.Points . mapM_ GL.vertex . plot $ pts

    pointSize $= 3
    sequence_ $ zipWith f gs colors

    let text2D x y s = do
        rasterPos (Vertex2 x (y::GLdouble))
        renderString Helvetica12 s

    setColor 0.5 0.5 0.5
    text2D a2 b1 (show i)
    text2D a1 b2 (show j)


scatterWindow name sz exs coor  = do
    w <- evWindow coor name sz (Just disp) kbd
    return w
  where n = dim . fst . head $ exs
        disp rdesi = do
            coord <- get rdesi
            scatter exs coord

        kbd rdesi (SpecialKey KeyUp) Down _ _ = do
            (i,j) <- get rdesi
            rdesi $= (i,(j+1) `mod` n)
            postRedisplay Nothing
        kbd rdesi (SpecialKey KeyDown) Down _ _ = do
            (i,j) <- get rdesi
            rdesi $= (i, (j-1) `mod`n)
            postRedisplay Nothing
        kbd rdesi (SpecialKey KeyRight) Down _ _ = do
            (i,j) <- get rdesi
            rdesi $= ((i+1)`mod`n,j)
            postRedisplay Nothing
        kbd rdesi (SpecialKey KeyLeft) Down _ _ = do
            (i,j) <- get rdesi
            rdesi $= ((i-1) `mod` n,j)
            postRedisplay Nothing
        kbd _ a b c d = kbdQuit a b c d

-----------------------------------------------------------------------

getIntegral o name = fromIntegral `fmap` (getParam o name :: IO Int)

regionDetector :: String -> IO Channels -> IO (IO (Channels, Maybe (Point,[Point])))
regionDetector "" cam = do
    o <- createParameters [("kb",intParam 60  0 255),
                           ("kg",intParam 100 0 255),
                           ("kw",intParam 200 0 255),
                           ("c1",intParam 7 1 9),
                           ("c2",intParam 8 1 9)]
    return $ do
        kb <- getIntegral o "kb"
        kg <- getIntegral o "kg"
        kw <- getIntegral o "kw"
        c1 <- getIntegral o "c1"
        c2 <- getIntegral o "c2"
        orig <- cam
        let col = detectRange c1 c2 . hsvCode kb kg kw . hsv $ orig
            rawconts = map (pixelsToPoints (size col). fst3) $
                       sortBy (compare `on` (negate.fst2)) $
                       contours 5 200 1 True col
            conts = map momentsContour rawconts
            p = case conts of
                    [] -> Nothing
                    (x,y,_,_,_):_ -> Just (Point x y, head rawconts)
        return (orig, p)


regionDetector name cam = do
    e <- evWindow () name (mpSize 8) Nothing (const kbdQuit)
    cam' <- regionDetector "" cam
    return $ do
        (orig,pc) <- cam'
        inWin e $ do
            drawImage (rgb orig)
            setColor 1 1 1
            pointSize $= 5
            pointCoordinates (size (rgb orig))
            case pc of
                Just (p,cont) -> do
                    renderPrimitive Points $ mapM_ vertex [p]
                    renderPrimitive LineLoop $ mapM_ vertex cont
                Nothing -> return ()
        return (orig, pc)

detectRange a b = thresholdVal8u b 0 IppCmpGreater . thresholdVal8u a 0 IppCmpLess

fst3 (a,_,_) = a
fst2 (_,a,_) = a

----------------------------------------------------------------



-- | to do
regionTracker :: String -> IO (Channels,Maybe (Point,[Point])) -> IO (IO (Channels, (Point,(Double,Double))))
regionTracker "" detector = do
    r <- newIORef s0
    return $ do
        (orig,p) <- detector
        st <- get r
        let st'@(State x c) =
                case p of
                    Nothing -> blindKalman sys st
                    Just (Point x y,_)  -> kalman sys st (vector [x, y])
                    --Nothing -> blindUKF sys' st
                    --Just (Point x y,_)  -> ukf sys' st (vector [x, y])
        r $= st'
        let pt = Point (x@>0) (x@>1)
            v = (x@>2,x@>3)
        return (orig, (pt,v))

regionTracker name detector = do
    det <- regionTracker "" detector
    e <- evWindow () name (mpSize 8) Nothing (const kbdQuit)
    return $ do
        (orig, (pt@(Point x y),v@(vx,vy))) <- det
        let pt2 = Point (x+vx) (y+vy)
        inWin e $ do
            drawImage (rgb orig)
            setColor 1 1 1
            pointSize $= 5
            pointCoordinates (size (rgb orig))
            renderPrimitive Points $ mapM_ vertex [pt]
            setColor 1 0 0
            lineWidth $= 2
            renderPrimitive Lines $ mapM_ vertex [pt,pt2]
        return (orig, (pt,v))

-- Kalman filter for 2D position and velocity

vector l = fromList l :: Vector Double
matrix ls = fromLists ls :: Matrix Double
diagl = diag . vector

f = matrix [[1,0,1,0],
            [0,1,0,1],
            [0,0,1,0],
            [0,0,0,1]]

h = matrix [[1,0,0,0],
            [0,1,0,0]]

q = 1 * diagl [1,1,1,1]

r = 2 * diagl [1,1]

s0 = State (vector [0, 0, 0, 0]) (diagl [1, 1, 1, 1])

sys = LinearSystem f h q r

------------------------------ ekf's check --------------

f' [x,y,vx,vy] = [ x+vx
                 , y+vy
                 , vx
                 , vy ]

h' [x,y,vx,vy ] = [x, y]

sys' = System f' h' q

-----------------------------------------------------------------------

-- | Creates a panoramic view from two cameras with (nearly) common camera center.
--
-- Left click: optimize
-- z: restart from identity
-- o: end optimization
panoramic :: Size              -- ^ of monitor window
          -> Double            -- ^ focal of base camera
          -> Double            -- ^ focal of source camera
          -> Double             -- ^ focal of virtual camera
          -> IO a               -- ^ base camera
          -> IO a               -- ^ source camera
          -> (a -> ImageFloat)  -- ^ how to extract the 'true image' from the cameras
          -> (a -> ImageFloat)           -- ^ how to extract the first argument to the similarity function
          -> (a -> ImageFloat)           -- ^ how to extract the second argument to the similarity function
          -> (ImageFloat -> ImageFloat -> Double) -- ^ the cost function
          -> IO (IO ImageFloat) -- ^ resulting virtual camera
panoramic sz fi1 fi2 fo camBase camAdj sel fa fb simil = do
    wMon <- evWindow (False,[0,0,0]) "autopanoramic" sz Nothing (mouse kbdQuit)
    wDeb <- evWindow () "debug" (mpSize 5) Nothing (const kbdQuit)
    wWar <- warper sz "control"
    return $ do
        img0raw <- camBase
        img1raw <- camAdj

        let img0 = sel img0raw
            img1 = sel img1raw

        (rh,_) <- getW wWar
        hi <- rh

        (opt,[pi,ti,ri]) <- getW wMon
        let [pan,tilt,roll] = if opt
                                then findRot (similgen fa fb simil) img0raw fi1 img1raw fi2 pi ti ri
                                else [pi,ti,ri]
            h = conjugateRotation pan tilt roll fi2 fi1
        putW wMon (opt,[pan,tilt,roll])
        let base = warp 0 (size img0) (hi<>kgen (fo/fi1)) img0
        warpOn (hi<>kgen (fo/fi1)<>h) base img1
        inWin wMon $ drawImage base
        inWin wDeb $ drawImage (fb img1raw)
        return base
  where
    -- click to adjust
    mouse _ st (MouseButton LeftButton) Down _ _ = do
        (_,p) <- get st
        st $= (True,p)

    -- restart from identity
    mouse _ st (Char 'z') Down _ _ = do
        st $= (True,[0,0,0])

    -- end optimization
    mouse _ st (Char 'o') Down _ _ = do
        (_,p) <- get st
        st $= (False,p)

    mouse def _ a b c d = def a b c d



similgen fa fb dab a h b = if ok roi then simil0 else 1E10
    where ia = fa a
          ib = fb b
          p = warp 0 (size ia) h ib
          roi = effectiveROI (size ia) h
          ok r = r1 r >= 0 && r2 r > 50 + r1 r && c1 r >= 0 && c2 r > 50 + c1 r
          simil0 = k * dab (f ia) (f p) --  sum32f (abs32f (f ia |*| f p))
              where f = modifyROI (const roi)
                    k = recip $ fromIntegral $ roiArea (f ia)

effectiveROI sz h = newroi where
    r = 3/4
    trrec = pointsToPixels sz . map lp $ ht h [[1,-r], [1,r], [-1, r], [-1, -r]]
    newroi = intersection (fullroi sz)
                    ROI {r1 = (minimum $ map row trrec), c1 = (minimum $ map col trrec),
                         r2 = (maximum $ map row trrec), c2 = (maximum $ map col trrec) }

    fullroi (Size h w) = ROI {r1=0, r2=h-1, c1=0, c2=w-1}
    lp [x,y] = Point x y




conjugateRotation pan tilt rho fi fo =
        kgen fo <> rot1 tilt <> rot2 pan <> rot3 rho <> kgen (1/fi)

cost simil a fa b fb [pan, tilt, roll] = simil a h b
    where h = conjugateRotation pan tilt roll fb fa

findRot simil a fa b fb pi ti ri = fst $ minimizeNMSimplex (cost simil a fa b fb) [pi,ti,ri] [0.1*degree, 0.1*degree,0.1*degree] 1E-3 30


------------------------------UKF pose tracker ----------------------------------

systemNoise = map (/1000) [0,0,0,0,0,0,0.1,0.1,0.1,0.01,0.01,0.01]

poseDyn world = System syspose (obspose world) (diagl (0.0001: systemNoise)) ((2/640) .* ident (2*length world))
    where syspose [f,p,t,r,cx,cy,cz,vx,vy,vz,vp,vt,vr] = [f,p+vp,t+vt,r+vr,cx+vx,cy+vy,cz+vz,vx,vy,vz,vp,vt,vr]
          obspose world pars = concat $ ht (syntheticCamera ( list2cam . take 7 $  pars)) (map (++[0]) world)

poseDynWithF f world = System syspose (obspose world) (diagl systemNoise) ((2/640) .* ident (2*length world))
    where syspose [p,t,r,cx,cy,cz,vx,vy,vz,vp,vt,vr] = [p+vp,t+vt,r+vr,cx+vx,cy+vy,cz+vz,vx,vy,vz,vp,vt,vr]
          obspose world pars = concat $ ht (syntheticCamera ( list2cam . (f:) . take 6 $  pars)) (map (++[0]) world)

-- Unscented Kalman filter for pose tracking from a planar reference
poseTracker :: String -> Maybe Double -> [[Double]] -> IO(Channels,[([Point],CameraParameters)]) 
            -> IO (IO(Channels, CameraParameters, (Vector Double, Matrix Double), Maybe ([Point],CameraParameters)))
poseTracker "" mbf ref cam = do
    let sys = case mbf of
                Nothing -> poseDyn ref
                Just f -> poseDynWithF f ref
        initst = case mbf of
                   Nothing -> State (vector [2,0,0,0,0,0,0,0,0,0,0,0,0]) (diagl [3,1,1,1,10,10,10,10,10,10,5,5,5])
                   Just f -> State (vector [0,0,0,0,0,0,0,0,0,0,0,0]) (diagl [1,1,1,10,10,10,10,10,10,5,5,5])
    r <- newIORef (False, initst)
    return $ do
        (orig,det) <- cam
        (ok,st@(State p c)) <- get r
        let rects = map fst det
            ps    = map (snd) det
            obs = vector $ concat $ map pl $ head $ rects
            obscam = case mbf of
                        Nothing -> vector $ (cam2list $ head ps) ++ [0,0,0,0,0,0]
                        _       -> vector $ (drop 1 $ cam2list $ head ps) ++ [0,0,0,0,0,0]

            (ok', State st' err) = case (ok, not (null rects)) of
                (False,False) -> (False, st)
                (False,True)  -> (True, State obscam c)
                (True,False)  -> (True, blindUKF sys st)
                (True,True)   -> (True, ukf sys st obs)

        r $= (ok', State st' err)
        let obs' = if null rects then Nothing else Just (head $ rects, head ps)
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


pl (Point x y) = [x,y]
cam2list (CamPar f p t r (x, y, z)) = [ f, p, t, r, x, y, z ]
list2cam [ f, p, t, r, x, y, z ] = (CamPar f p t r (x, y, z))
extractCam = list2cam . take 7 . toList

shPose (CamPar f p t r (cx,cy,cz)) = printf "f=%.2f  pan=%.1f  tilt=%.1f  roll=%.1f (%.1f, %.1f, %.1f)" 
                                            f (p*degree) (t*degree) (r*degree) (cx*cm) (cy*cm) (cz*cm)
    where degree = 180/pi
          cm = 10

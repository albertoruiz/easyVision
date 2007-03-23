-- experiments on automatic planar rectification

-- TO DO:
--   put online the demo video
--   average world fragments
--   user selected horizon by clicking on the error surface
--   semi-automatic selection of points

-- This should work with a firewire camera: 
--    $ ./a.out /dev/dv1394
-- or with a raw dv video, for instance:
--    $ wget http://ditec.um.es/~pedroe/svnvideos/misc/table.dv  # (not yet online)
--    $ ./frontal table.dv

module Main where

import EasyVision
import ImagProc.Ipp.Core(image,ImageFloat(F),fullroi,setData32f)
import Graphics.UI.GLUT hiding (Matrix, Size, Point)
import Data.IORef
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)
import Data.List(minimumBy, elemIndex)
import GSL hiding (size)
import Vision

-------------------------------------------------------

vector v = fromList v :: Vector Double

floorSize = 256

data MyState = ST { imgs :: [ImageFloat]      -- selected views
                  , corners, marked ::[Pixel] -- corners in the current image
                  , pts  :: [[Point]]         -- homologous points of the selected images
                  , hs   :: [Matrix Double]   -- interimage homographies
                  , cam0 :: Matrix Double     -- solution
                  , drfuns :: [IO()]          -- camera drawing functions
                  , cost  :: Maybe ImageFloat -- the cost function
                  , zoom :: Double            -- visualization scale of the rectified image
                  , priorInfo :: KnownFs      -- available info about the fs
                  , baseView ::Int            -- index of the desired base view 
                  , targetView :: Int         -- index of any other selected view
                  , trackball :: IO ()        -- viewpoint generator
                  , camera :: IO(ImageYUV)    -- the camera
                  }

--------------------------------------------------------------
main = do
    args <- getArgs
    let sz = Size 288 384
    (cam,ctrl) <- mplayer (args!!0) sz >>= withPause

    (tb,kc,mc) <- newTrackball

    state <- prepare ST { imgs=[]
                        , corners=[], marked = []
                        , pts=[]
                        , hs = []
                        , cam0 = ident 3
                        , cost = Nothing
                        , drfuns=[]
                        , priorInfo = AllKnown (repeat 2.8)
                        , baseView = 0, targetView = 0
                        , zoom = 0.2
                        , trackball = tb
                        , camera = cam
                        }

    addWindow "camera" sz Nothing (marker (const (kbdcam ctrl))) state   -- shows the live video
    addWindow "base" sz Nothing keyboard state                           -- desired base view
    addWindow "selected" sz Nothing keyboard state                       -- target view warped into the base view
    addWindow "world" (Size floorSize floorSize) Nothing warpkbd state   -- combined metric rectification
    addWindow "cost" (Size 200 200) Nothing keyboard state               -- the cost function

    let modInfo info = do
        st <- readIORef state
        u <- compute $ (ust st) {priorInfo = info}
        writeIORef state st { ust = u }

    attachMenu LeftButton $ Menu
        [MenuEntry "Known f's"    (modInfo $ AllKnown (repeat 2.8))
        ,MenuEntry "Known f1"     (modInfo $ F1Known 2.8)
        ,MenuEntry "Constant f"   (modInfo ConstantUnknown)
        ,MenuEntry "Variable f's" (modInfo AllUnknown)
        ]

    addWindow "3D view" (Size 600 600) Nothing keyboard state     -- 3D representation
    keyboardMouseCallback $= Just (kc (keyboard state))
    motionCallback $= Just mc

    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureFunction $= Replace
    depthFunc $= Just Less

    launch state (worker cam)

-------------------------------------------------------------------

encuadra h = desp (-a,-b) where
    [a,b] = toList $ inHomog $ h <> vector [0,0,1]

genInterimage views = map (estimateHomography (head views)) (id views)

explore n dr dy (r,y) fun = (argmin, partit n vals) where
    a = toList $ linspace n (r-dr,r+dr)
    b = toList $ linspace n (y-dy,y+dy)
    args = [ (r',y') | r' <- a, y' <- b]
    vals = map (fromRational.toRational.fun) args
    argmin = args!!posMin vals
    posMin l = k where Just k = elemIndex (minimum l) l

rotateList n list = take (length list) $ drop n $ cycle list

---------------------------------------------------------------------

pl (Point x y) = [x,y]
mpl = map pl

lp [x,y] = Point x y
mlp = map lp

htp h = ht h . mpl

worker cam inWindow st = do
    camera <- cam >>= yuvToGray >>= scale8u32f 0 1
    hotPoints <- getCorners 1 7 0.1 200 camera

    inWindow "camera" $ do
        drawImage camera
        pixelCoordinates (size camera)
        setColor 1 0 0
        pointSize $= 3
        renderPrimitive Points (mapM_ vertex hotPoints)
        setColor 0 0 1
        renderPrimitive Points (mapM_ vertex (marked st))

    let n = length (imgs st)

    let bv = baseView st
    let sv = targetView st
    let ps = pts st !! bv
    let h = inv (hs st !! bv) <> (hs st !! sv) -- from selected to base

    when (n > 0) $ do
        inWindow "base" $ do                   -- base view
            drawImage $ imgs st !! bv
            pointCoordinates (Size 3 4)
            setColor 0.75 0 0
            renderPrimitive LineLoop (mapM_ vertex ps)
            pointSize $= 5
            renderPrimitive Points (mapM_ vertex ps)

    when (n > 1) $ do

        inWindow "selected" $ do            -- selected view warped into the base view
            w <- warp (Size 288 384) h (imgs st !! sv)
            drawImage w

    when (length (drfuns st) > 1) $ do

        let r = scaling (zoom st) <> inv (cam0 st) -- hmm
        w@(F i) <- image (Size floorSize floorSize)
        set32f 0.25 (fullroi i) w
        let g im h = warpOn (r <> h) w im
        sequence_ $ reverse $ rotateList sv $ zipWith g (imgs st) (hs st)

        inWindow "world" $ do         -- automatically rectified plane
            drawImage w

        case cost st of
            Nothing -> return ()
            Just c  -> inWindow "cost" (drawImage c)

        inWindow "3D view" $ do             -- 3D representation
            clear [ColorBuffer, DepthBuffer]
            trackball st

            let ref = htp (inv (cam0 st)) (pts st !!0) -- the rectified homologous points
            let wref = htp r (pts st !!0)              -- this same points in image w
            let hx = estimateHomography ref wref

            drawTexture w $ map (++[-0.01]) $ ht hx [[1,1],[-1,1],[-1,-1],[1,-1]]

            setColor 0 0 1
            lineWidth $= 2
            renderPrimitive LineLoop $ mapM_ vertex ref

            let drawcams = rotateList sv (drfuns st)
            setColor 1 1 1
            lineWidth $= 1
            sequence_ (tail drawcams)

            setColor 1 0 0
            head drawcams

    return st {corners = hotPoints}

--------------------------------------------------------------------------

compute st = do
    let info = priorInfo st

    let uhs = tail (hs st)
    let f = consistency info uhs

    let explsz = 50                  -- coarse exploration of the error surface
    costsurf' <- image (Size 50 50)
    let (minim,cost) = explore 50 (60*degree) 3 (0,2) ((0.5*).f)
    setData32f costsurf' cost
    costsurf <- resize32f (Size 200 200) costsurf'

    let [rho,yh] = fst $ findSol f minim     -- detailed optimization from the minimum
    let (c0,_) = extractInfo info uhs (rho, yh)
    let r0 = inv (c0<> diag (vector[-1,1,1])) -- move to Autofrontal.hs ?
    let c0' = inv $ encuadra r0 <> r0

    print(rho, yh, f(rho,yh))
    mapM_ (print . focalFromHomogZ0 . (<> c0). inv) uhs
    --writeFile "real.txt" . show . foldr1 (<->) $ uhs

    let cameras = map (cameraFromHomogZ0 Nothing. (<>c0'). inv) (hs st)
    textures <- mapM (extractSquare 64) (imgs st)
    let drs = [drawCamera 0.4 c (Just t) | (Just c,t) <- zip cameras textures]

    return st { cam0 = c0'
              , drfuns = drs
              , cost = Just costsurf
              }

------------------------------------------------------

compareBy f = (\a b-> compare (f a) (f b))

closest [] p = p
closest hp p = minimumBy (compareBy $ dist p) hp
    where dist (Pixel a b) (Pixel x y) = (a-x)^2+(b-y)^2

-----------------------------------------------------------------
-- callbacks
-----------------------------------------------------------------

keyboard st (SpecialKey KeyUp) Down _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {baseView = min (length (imgs $ ust s)-1) $ baseView (ust s) + 1}}
keyboard st (SpecialKey KeyDown) Down _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {baseView = max 0 $ baseView (ust s) - 1}}

keyboard st (SpecialKey KeyRight) Down _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {targetView = min (length (imgs $ ust s)-1) $ targetView (ust s) + 1}}
keyboard st (SpecialKey KeyLeft) Down _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {targetView = max 0 $ targetView (ust s) - 1}}

keyboard _ _ _ _ _ = return ()

------------------------------------------------------------------

marker _ str (MouseButton LeftButton) Down _ pos@(Position x y) = do
    st <- readIORef str
    let u = ust (st)
    let newpoint = closest (corners u) (Pixel (fromIntegral y) (fromIntegral x))
    let m = newpoint : marked u
    if (length m /= 4)
        then writeIORef str st { ust = u {marked = m} }
        else do
            camera <- camera (ust st) >>= yuvToGray
            let hp = pixelsToPoints (size camera) m
            im  <- scale8u32f 0 1 camera
            let u' = u { marked = []
                       , imgs = imgs u ++ [im]
                       , pts = pts u ++ [hp]
                       , hs = genInterimage (map (mpl) (pts u'))
                       , targetView = length (imgs u') -1
                       }
            v <- compute u' -- automatically updated on the fourth point
            writeIORef str st { ust = v }

marker _ st (MouseButton RightButton) Down _ pos@(Position x y) = do
    modifyIORef st $ \s -> s {ust = (ust s) {marked = case marked (ust s) of
                                                        [] -> []
                                                        _:t -> t }}

marker def st b s m p = def st b s m p

--------------------------------------------------------------------

warpkbd st (MouseButton WheelUp) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {zoom = zoom (ust s) /1.1}}

warpkbd st (MouseButton WheelDown) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {zoom = zoom (ust s) *1.1}}

warpkbd st b s m p = keyboard st b s m p

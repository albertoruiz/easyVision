-- experiments on automatic planar rectification

-- TO DO:
--   put online the demo video
--   average world fragments
--   online selection of the prior calibration info

-- This should work with a firewire camera: 
--    $ ./a.out /dev/dv1394
-- or with a raw dv video, for instance:
--    $ wget http://ditec.um.es/~pedroe/svnvideos/misc/table.dv  # (not yet online)
--    $ ./frontal table.dv

module Main where

import Ipp hiding (shift)
import Graphics.UI.GLUT hiding (Matrix)
import Data.IORef
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)
import Data.List(minimumBy, elemIndex)
import GSL
import Vision

-------------------------------------------------------

type Point = [Double]  -- provisional
type Pixel = [Int]

floorSize = 256

data MyState = ST { imgs :: [Img]             -- selected views
                  , corners, marked ::[Point] -- corners in the current image
                  , pts  :: [[Point]]         -- homologous points of the selected images
                  , hs   :: [Matrix]          -- interimage homographies
                  , cam0 :: Matrix            -- solution
                  , cams :: [Matrix]          -- estimated cameras
                  , drfuns :: [IO()]          -- drawing functions of floor and cameras
                  , cost  :: Maybe Img        -- the cost function
                  , zoom :: Double            -- visualization scale of the rectified image

                  , new    :: Bool            -- a new view has been selected
                  , baseView ::Int            -- index of the desired base view 
                  , targetView :: Int         -- index of any other selected view

                  , trackball :: IO ()        -- viewpoint generator
                  }

--------------------------------------------------------------
main = do
    args <- getArgs
    cam@(_,_,(h,w)) <- openCamera (args!!0) Gray (288,384)

    (tb,kc,mc) <- newTrackball

    state <- prepare cam ST { imgs=[]
                            , corners=[], marked = []
                            , pts=[]
                            , cams=[], hs = []
                            , cam0 = ident 3
                            , cost = Nothing
                            , drfuns=[]

                            , new = False

                            , baseView = 0, targetView = 0

                            , zoom = 0.2

                            , trackball = tb

                            }

    addWindow "camera" (w,h) Nothing marker state            -- shows the live video
    addWindow "base" (w,h) Nothing keyboard state            -- desired base view
    addWindow "selected" (w,h) Nothing keyboard state        -- target view warped into the base view
    addWindow "world" (floorSize,floorSize) Nothing warpkbd state     -- combined metric rectification
    addWindow "cost" (100,100) Nothing keyboard state        -- the cost function

    addWindow "3D view" (600,600) Nothing keyboard state     -- 3D representation
    keyboardMouseCallback $= Just (kc (keyboard state))
    motionCallback $= Just mc

    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureFunction $= Decal
    depthFunc $= Just Less

    launch state worker

-------------------------------------------------------------------

encuadra h = desp (-a,-b) where
    [a,b] = toList $ inHomog $ h <> realVector [0,0,1]

genInterimage views = map (estimateHomography (head views)) (id views)

explore n dr dy (r,y) fun = (argmin, partit n vals) where
    a = toList $ linspace n (r-dr,r+dr)
    b = toList $ linspace n (y-dy,y+dy)
    args = [ (r',y') | r' <- a, y' <- b]
    vals = map (fromRational.toRational.fun) args
    argmin = args!!posMin vals
    posMin l = k where Just k = elemIndex (minimum l) l

rotate n list = take (length list) $ drop n $ cycle list

---------------------------------------------------------------------

worker inWindow camera st@ST{new=False} = do
    hotPoints <- getCorners 1 7 0.1 200 camera

    inWindow "camera" $ do
        drawImage camera
        pixelCoordinates (384,288)
        mycolor 1 0 0
        pointSize $= 3
        renderPrimitive Points (vertices hotPoints)
        mycolor 0 0 1
        renderPrimitive Points (vertices (marked st))


    let n = length (imgs st)

    when (n > 1) $ do
        let bv = baseView st
        let sv = targetView st
        let ps = pts st !! bv
        let h = inv (hs st !! bv) <> (hs st !! sv) -- from selected to base

        inWindow "base" $ do                -- base view
            drawImage $ imgs st !! bv
            pointCoordinates (4,3)
            mycolor 0.75 0 0
            renderPrimitive LineLoop (vertices ps)
            pointSize $= 5
            renderPrimitive Points (vertices ps)

        inWindow "selected" $ do            -- selected view warped into the base view
            w <- warp (288,384) h (imgs st !! sv)
            drawImage w

        let r = scaling (zoom st) <> inv (cam0 st) -- hmm
        w <- img I32f floorSize floorSize
        set32f 0.25 w (fullroi w)
        let g im h = warpOn (r <> h) w im
        sequence_ $ reverse $ Main.rotate sv $ zipWith g (imgs st) (hs st)

        inWindow "world" $ do         -- automatically rectified plane
            drawImage w

        case cost st of
            Nothing -> return ()
            Just c  -> inWindow "cost" (resize32f (100,100) c >>= drawImage)

        inWindow "3D view" $ do             -- 3D representation
            clear [ColorBuffer, DepthBuffer]
            trackball st

            let ref = ht (inv (cam0 st)) (pts st !!0) -- the rectified homologous points
            let wref = ht r (pts st !!0)              -- this same points in image w
            let hx = estimateHomography ref wref

            let norw = pixel2pointTrans w

            drawTexture w $ map (++[-0.01]) $ ht hx [[1,1],[-1,1],[-1,-1],[1,-1]]

            mycolor 0 0 1
            lineWidth $= 2
            renderPrimitive LineLoop $ vertices ref

            let drawcams = Main.rotate sv (drfuns st)
            mycolor 1 1 1
            lineWidth $= 1
            sequence_ (tail drawcams)

            mycolor 1 0 0
            head drawcams


    return st {corners = hotPoints}

--------------------------------------------------------------------------

worker inWindow camera st@ST{ new=True
                            , marked = m
                            , pts = ps
                            , imgs = ims
                            , cams = cs
                            , drfuns = funs } = do
    let hp = pixel2point camera m
    im  <- scale8u32f 0 1 camera
    imtext <- extractSquare 128 im
    let images = ims ++ [im]
    let points = ps ++ [hp]
    let interhomog = genInterimage points

    let info = AllKnown (repeat 2.8)
    --let info = F1Known 2.8
    --let info = ConstantUnknown

    let uhs = tail interhomog
    let f = consistency info uhs

    let explsz = 50                  -- coarse exploration of the error surface
    costsurf <- img I32f 50 50
    let (minim,cost) = explore 50 (60*degree) 3 (0,2) f
    setData32f costsurf cost

    let [rho,yh] = fst $ findSol f minim     -- detailed optimization from the minimum
    let (c0,_) = extractInfo info uhs (rho, yh)
    let r0 = inv (c0<> diag (realVector[-1,1,1])) -- move to Autofrontal.hs ?
    let c0' = inv $ encuadra r0 <> r0

    print(rho, yh, f(rho,yh))
    mapM_ (print . focalFromHomogZ0 . (<> c0). inv) uhs
    --writeFile "real.txt" . show . foldr1 (<->) $ uhs

    let cameras = map (cameraFromHomogZ0 Nothing. (<>c0'). inv) interhomog
    textures <- mapM (extractSquare 64) images
    let drs = [drawCamera 0.4 c t | (Just c,t) <- zip cameras textures]

    return st { new=False
              , marked = []
              , pts = points
              , imgs = images
              , hs = interhomog
              , targetView = length images -1
              , cam0 = c0'
              , drfuns = drs
              , cost = Just costsurf
              }

------------------------------------------------------

compareBy f = (\a b-> compare (f a) (f b))

closest [] p = p
closest hp p = minimumBy (compareBy $ dist p) hp
    where dist [a,b] [x,y] = (a-x)^2+(b-y)^2

-----------------------------------------------------------------
-- callbacks
-----------------------------------------------------------------
keyboard st (Char 'p') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}
keyboard st (Char ' ') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}
keyboard _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess

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

marker str (MouseButton LeftButton) Down _ pos@(Position x y) = do
    st <- readIORef str
    let u = ust (st)
    let newpoint = closest (corners u) $ map fromIntegral [x,y]
    let m = newpoint : marked u
    writeIORef str st { ust = u {marked = m, new = length m == 4} }

marker st (MouseButton RightButton) Down _ pos@(Position x y) = do
    modifyIORef st $ \s -> s {ust = (ust s) {marked = tail $ marked (ust s) }}

marker st b s m p = keyboard st b s m p

--------------------------------------------------------------------

warpkbd st (MouseButton WheelUp) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {zoom = zoom (ust s) /1.1}}

warpkbd st (MouseButton WheelDown) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {zoom = zoom (ust s) *1.1}}

warpkbd st b s m p = keyboard st b s m p

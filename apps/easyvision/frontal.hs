-- experiments on automatic planar rectification

-- TO DO:
--   put online the demo video
--   average world fragments

-- This should work with a firewire camera: 
--    $ ./a.out /dev/dv1394
-- or with a raw dv video, for instance:
--    $ wget http://ditec.um.es/~pedroe/svnvideos/misc/table.dv  # (not yet online)
--    $ ./frontal table.dv

module Main where

import Ipp hiding (shift)
import Graphics.UI.GLUT hiding (Matrix, drawPixels)
import Data.IORef
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)
import Data.List(minimumBy)
import GSL
import Vision

type Point = [Double]  -- provisional
type Pixel = [Int]

floorSize = 256

data MyState = ST { imgs :: [Img]             -- selected views
                  , corners, marked ::[[Int]] -- corners in the current image 
                  , pts  :: [[Point]]         -- homologous points of the selected images
                  , hs   :: [Matrix]          -- interimage homographies
                  , cam0 :: Matrix            -- solution
                  , cams :: [Matrix]          -- estimated cameras
                  , drfuns :: [IO()]          -- drawing functions of floor and cameras
                  , world :: Maybe Img        -- the reconstruction

                  , new    :: Bool            -- a new view has been selected
                  , baseView ::Int            -- index of the desired base view 
                  , targetView :: Int         -- index of any other selected view

                  , angle, angle' :: Double   -- provisional state for the viewpoint of the 3D view
                  , zoom :: Double            -- visualization scale of the rectified image
                  , dist:: Double             -- distance in the 3d view
                  }

--------------------------------------------------------------
main = do
    args <- getArgs
    cam@(_,_,(h,w)) <- openCamera (args!!0) Gray (288,384)

    state <- prepare cam ST { imgs=[]
                            , corners=[], marked = []
                            , pts=[]
                            , cams=[], hs = []
                            , cam0 = ident 3
                            , world = Nothing
                            , drfuns=[]

                            , new = False

                            , baseView = 0, targetView = 0

                            , angle = 0, angle' = 0
                            , zoom = 0.2
                            , dist = 10
                            }

    addWindow "camera" (w,h) Nothing marker state            -- shows the live video
    addWindow "base" (w,h) Nothing keyboard state            -- desired base view
    addWindow "selected" (w,h) Nothing keyboard state        -- target view warped into the base view
    addWindow "world" (floorSize,floorSize) Nothing warpkbd state     -- combined metric rectification

    addWindow "3D view" (600,600) Nothing keyboard state     -- 3D representation
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureFunction $= Decal

    launch state worker

-------------------------------------------------------------------

encuadra h = desp (-a,-b) where
    [a,b] = toList $ inHomog $ h <> realVector [0,0,1]

genInterimage views = map (estimateHomography (head views)) (id views)

getCorners camera = do
    let suaviza = 1 `times` gauss Mask5x5

    im  <- scale8u32f 0 1 camera
    h <- suaviza im >>= hessian >>= scale32f (-1.0)

    (mn,mx) <- Ipp.minmax h
    hotPoints <- localMax 7 h >>= thresholdVal32f (mx/10) 0.0 ippCmpLess >>= getPoints32f 200
    return hotPoints

environment n dr dy (r,y) fun = reshape n $ realVector vals where
    a = toList $ linspace n (r-dr,r+dr)
    b = toList $ linspace n (y-dy,y+dy)
    vals = [ fun (r',y') | r' <- a, y' <- b]

rotate n list = take (length list) $ drop n $ cycle list

---------------------------------------------------------------------

worker inWindow camera st@ST{new=False} = do
    hotPoints <- getCorners camera

    inWindow "camera" $ do
        display camera
        mycolor 1 0 0
        drawPixels hotPoints
        mycolor 0 0 1
        drawPixels (marked st)

    let n = length (imgs st)

    when (n > 0) $ do
        let bv = baseView st
        let sv = targetView st
        let ps = pts st !! bv
        let h = inv (hs st !! bv) <> (hs st !! sv) -- from selected to base

        inWindow "base" $ do                -- base view
            display $ imgs st !! bv
            mycolor 0 1 0
            drawPoints ps

        inWindow "selected" $ do            -- selected view warped into the base view
            w <- warp (288,384) h (imgs st !! sv)
            display w

        let r = scaling (zoom st) <> inv (cam0 st) -- hmm
        w <- img I32f floorSize floorSize
        set32f 0 w (fullroi w)
        let g im h = warpOn (r <> h) w im
        sequence_ $ Main.rotate sv $ zipWith g (imgs st) (hs st)

        inWindow "world" $ do         -- automatically rectified plane
            display w

        drwfloor <- genDrawTexture (width w) w 

        inWindow "3D view" $ do             -- 3D representation
            setView st

            let ref = ht (inv (cam0 st)) (pts st !!0) -- the rectified homologous points
            let wref = ht r (pts st !!0)              -- this same points in image w
            let hx = estimateHomography ref wref

            let norw = pixel2pointTrans w

            drwfloor $ map (++[0]) $ ht hx [[1,1],[-1,1],[-1,-1],[1,-1]]


            mycolor 0 0 1
            lineWidth $= 2
            let f [x,y] = Vertex2 x y
            renderPrimitive LineLoop $ mapM_ (vertex.f) ref

            mycolor 1 1 1
            lineWidth $= 1
            sequence_ (drfuns st)

    return st {corners = hotPoints}

--------------------------------------------------------------------------

worker inWindow camera st@ST{ new=True
                            , marked = m
                            , pts = ps
                            , imgs = ims
                            , cams = cs
                            , drfuns = funs } = do
    let fix = pixel2point camera
    let hp = reverse (fix m)
    im  <- scale8u32f 0 1 camera
    let images = ims ++ [im]
    let points = ps ++ [hp]
    let interhomog = genInterimage points

    let info = AllKnown (repeat 2.8)
    -- let info = F1Known 2.8
    -- let info = ConstantUnknown

    let uhs = tail interhomog
    let f = consistency info uhs
    let [rho,yh] = fst $ findSol f (0, 2)
    let (c0,_) = extractInfo info uhs (rho, yh)
    let r0 = inv (c0<> diag (realVector[-1,1,1])) -- move to Autofrontal.hs ?
    let c0' = inv $ encuadra r0 <> r0

    print(rho, yh, f(rho,yh))
    mapM_ (print . focalFromHomogZ0 . (<> c0). inv) uhs
    when False $ do
        writeFile "real.txt" . show . foldr1 (<->) $ uhs
        imshow $ environment 100 (20*degree) 0.5 (rho,yh) f

    let cameras = map (cameraFromHomogZ0 Nothing. (<>c0'). inv) interhomog
    drs <- sequence [genDrawCamera 0.4 256 c i | (Just c,i) <- zip cameras images]

    return st { new=False
              , marked = []
              , pts = points
              , imgs = images
              , hs = interhomog
              , targetView = length images -1
              , cam0 = c0'
              , drfuns = drs
              }

------------------------------------------------------------------

setView st = do
    clear [ColorBuffer]
    matrixMode $= Projection
    loadIdentity
    perspective 60 1 1 100
    let ang = angle st
    let ang' = angle' st
    let d = dist st
    lookAt (Vertex3 (d*sin ang*sin ang')
                    (d*sin ang*cos ang')
                    (d*cos ang))
           (Vertex3 0 0 0)
           (Vector3 0 1 0)
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

keyboard st (MouseButton WheelUp) _ (Modifiers{ctrl = Down}) _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {dist = dist (ust s) *1.1}}
keyboard st (MouseButton WheelDown) _ (Modifiers{ctrl = Down}) _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {dist = dist (ust s) /1.1}}

keyboard st (MouseButton WheelUp) _ (Modifiers{shift = Down}) _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {angle' = angle' (ust s) + 1*degree}}
keyboard st (MouseButton WheelDown) _ (Modifiers{shift = Down}) _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {angle' = angle' (ust s) -1*degree}}

keyboard st (MouseButton WheelUp) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {angle = angle (ust s) + 1*degree}}
keyboard st (MouseButton WheelDown) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {angle = angle (ust s) -1*degree}}

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
    let newpoint = closest (corners u) $ map fromIntegral [y,x]
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

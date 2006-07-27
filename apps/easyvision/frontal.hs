-- TO DO:
--   put online the demo video
--   clear earlier points
--   clean up callbacks
--   average world fragments
--   draw 3d scene with cameras

-- This should work with a firewire camera: 
--    $ ./a.out /dev/dv1394
-- or with a raw dv video, for instance:
--    $ wget http://ditec.um.es/~pedroe/svnvideos/misc/table.dv  # (not yet online)
--    $ ./frontal table.dv

module Main where

import Ipp hiding (shift)
import Graphics.UI.GLUT hiding (Matrix)
import Graphics.Rendering.OpenGL hiding (Matrix)
import Data.IORef
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)
import Data.List(minimumBy)
import GSL
import Vision

import Data.Bits ( (.&.) )


type Point = [Double]  -- provisional
type Pixel = [Int]

type ImageDrawer = [[Double]] -> IO ()

data MyState = ST { imgs :: [Img]
                  , pts  :: [[Point]]
                  , hs   :: [Matrix]
                  , cam0 :: Matrix

                  , smooth :: Int
                  , zoom :: Double
                  , marked ::[[Int]]
                  , new    :: Bool

                  , toshow :: Int
                  , basev ::Int

                  , suelo :: Maybe ImageDrawer
                  , angle :: Double

                  }

list =< elem = list ++ [elem]

mycolor r g b = currentColor $= Color4 r g (b::GLfloat) 1

encuadra h = desp (-a) (-b) where
    [a,b] = toList $ inHomog $ h <> realVector [0,0,1]

htr = ht :: Matrix -> [[Double]] -> [[Double]]

pixel2point (h, w) = (fix, adapt) where
    cm = fromIntegral $ w `quot` 2
    rm = fromIntegral $ h `quot` 2
    nor = desp (-1) (rm/cm) <> diag (realVector [1/cm,-1/cm,1])
    fix = htr nor . map (reverse . map fromIntegral)
    adapt h = toList $ inv nor <> h <> nor

--------------------------------------------------------------
main = do
    args <- getArgs
    cam@(_,_,(h,w)) <- openCamera (args!!0) Gray (288,384)

    state <- prepare cam ST { imgs=[]
                            , pts=[]
                            , hs = []
                            , cam0 = ident 3

                            , smooth = 1
                            , zoom = 0.2
                            , marked = []
                            , new = False

                            , toshow = 0
                            , basev = 0

                            , suelo = Nothing
                            , angle = 0

                            }

    addWindow "camera" (w,h) Nothing marker state
    --addWindow "hessian" (w,h) Nothing keyboard state
    addWindow "selected" (w,h) Nothing keyboard state
    addWindow "warped" (w,h) Nothing keyboard state
    addWindow "rectified" (w,h) Nothing keyboard state
    addWindow "world" (w,h) Nothing keyboard state
    addWindow "3D view" (400,400) (Just draw3d) keyboard state

    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureFunction $= Decal

    launch state worker

-------------------------------------------------------------------

worker inWindow camera st = do

    let (fix,adapt) = pixel2point (height camera, width camera)
    let suaviza = smooth st `times` gauss Mask5x5

    im  <- scale8u32f 0 1 camera
    h <- suaviza im >>= hessian >>= scale32f (-1.0)

    (mn,mx) <- Ipp.minmax h
    hotPoints <- localMax 7 h >>= thresholdVal32f (mx/10) 0.0 ippCmpLess >>= getPoints32f 200

    let newmark = new st 
    let st' = if newmark -- a new point is clicked by the user
                then     -- it is replaced by the closest hot point
                    let n:other = marked st
                    in st { new = False,
                            marked = closest hotPoints n : other }
                else st

    -- when there are four points we add a new view and all required info:
    let newimage = newmark && length (marked st') == 4
    let st'' = if newimage
                then st' { pts  = pts  st' =< fix (marked st'),
                           marked = [],
                           imgs = imgs st' =< im,
                           hs = genInterimage (pts st'')}
                else st'


    let info = AllKnown (repeat 2.8)
    -- let info = F1Known 2.8
    -- let info = ConstantUnknown

    let uhs = tail (hs st'')
    let f = consistency info uhs
    let [rho,yh] = fst $ findSol f (0, 2)
    let sol = extractInfo info uhs (rho, yh)

    let st = if newimage
               then st'' {cam0 = fst sol}
               else st''

    inWindow "camera" $ do
        display camera
        mycolor 1 0 0
        mydraw hotPoints
        mycolor 0 0 1
        mydraw (marked st)

    when False $ inWindow "hessian" $ do
        auxim <- copy32f im
        copyROI32f auxim h
        display auxim {vroi = vroi h}

    let images = imgs st
    let nviews = length images

    let ptget = (toshow st) `mod` nviews
    let pbase = (basev  st) `mod` nviews
    let pt = pts st!!ptget
    let pb = pts st!!pbase
    let ht =  hs st!!ptget
    let hb =  hs st!!pbase
    let imt = images!!ptget
    let imb = images!!pbase

    let t = inv hb <> ht

    let r0 = inv (cam0 st)
    let enc = encuadra r0
    let r = scaling (zoom st) <> enc <> r0

    when (length images>0) $ do

        inWindow "selected" $ do
            display imb
            mycolor 1 1 0
            mydraw' pb
        inWindow "warped" $ do
            warp (adapt t) imt >>= display

        w <- warp (adapt $ r <> ht) imt
        inWindow "rectified" $ do
            display w

        let g im h = warpOn (adapt $ r <> h) w im
        inWindow "world" $ do 
            sequence_ $ zipWith g images (hs st)
            display w

        when (newimage  && (length images>1)) $ do
            writeFile "real.txt" . show . foldr1 (<->) $ uhs
            print(rho, yh, f(rho,yh))
            mapM_ (print . focal . (<> cam0 st). inv) uhs
            when False $ do
                imshow $ environment 100 (20*degree) 0.5 (rho,yh) f

        inWindow "3D view" $ do
            postRedisplay Nothing

    if newimage
        then do
            f <- genDrawTexture 256 im
            return st {suelo = Just f}
        else return st

------------------------------------------------------
mydraw xs = do
    pointSize $= 3
    renderPrimitive Points $ mapM_ f xs where
        f [y,x] = vertex (Vertex2 (fromIntegral x::GLfloat) (288-1-fromIntegral y))

mydraw' xs = do
    pointSize $= 3
    matrixMode $= Projection
    loadIdentity
    ortho2D (-1) (1) (-0.75) (0.75)
    matrixMode $= Modelview 0
    loadIdentity
    renderPrimitive Points $ mapM_ (vertex.f) xs where
        f [x,y] = Vertex2 x y

------------------------------------------------------

--compareBy f = (\a b-> compare (f a) (f b))

closest [] p = p
closest hp p = minimumBy (compareBy $ dist p) hp
    where dist [a,b] [x,y] = (a-x)^2+(b-y)^2

genInterimage views = map (estimateHomography (head views)) (id views)


environment n dr dy (r,y) fun = reshape n $ realVector vals where
    a = toList $ linspace n (r-dr,r+dr)
    b = toList $ linspace n (y-dy,y+dy)
    vals = [ fun (r',y') | r' <- a, y' <- b]


----------------------------------------------------------------

shcam cam pts = (c,p) where 
    (h,f) = toCameraSystem cam
    t1 = h <> diag (realVector [1,1,1,3])
    c = ht t1 (cameraOutline f)
    t2 = t1 <> diag (realVector [1,1,1,f])
    p = ht t2 (map (++[f]) pts)

for l f = (flip mapM_) l f

draw3d st = do
    matrixMode $= Projection
    loadIdentity
    perspective 40 1 1 100
    let ang = angle (ust st)
    lookAt (Vertex3 0 (20*sin ang) (20*cos ang)) (Vertex3 0 0 0) (Vector3 0 1 0)

    let rawcam0 = cam0 (ust st)
    let fixcam0 = inv (encuadra (inv rawcam0) <> inv rawcam0)

    case suelo (ust st) of
        Nothing -> return ()
        Just f -> do
            f [[-4,  4, 0],
               [ 4,  4, 0],
               [ 4, -4, 0],
               [-4, -4, 0]]

    case pts (ust st) of
        [] -> return ()
        pts:_ -> do
            let f [x,y] = Vertex2 x y
            mycolor 0 0 1
            lineWidth $= 2
            let worldpoints =  (htr (inv fixcam0) pts)
            let flipx = diag (realVector [-1,1,1])
            renderPrimitive LineLoop $ mapM_ (vertex.f) worldpoints
            --let Just pars = poseGen Nothing (estimateHomography (htr (ident 3) pts) worldpoints)
            let pars = easyCamera' 40 (2,-2,2) (0,0,0) 0
            let (pts,coo) = shcam (syntheticCamera pars) [[-1,1],[1,1],[1,-1],[-1,-1]]
            let Just f = suelo (ust st)
            f coo
            lineWidth $= 1
            mycolor 1 0 0
            let f [x,y,z] = Vertex3 x y z
            renderPrimitive LineLoop $ mapM_ (vertex.f) pts
{-
    for (hs (ust st)) $ \h -> do
        
        let c' = inv h <> fixcam0
        let pars = poseGen Nothing c'
        case pars of
            Nothing -> return ()
            Just m -> do
                let (pts,coo) = shcam (syntheticCamera m) (map (map (*4)) [[-1,1],[1,1],[1,-1],[-1,-1]])
                lineWidth $= 1
                mycolor 1 0 0
                let f [x,y,z] = Vertex3 x y z
                renderPrimitive LineLoop $ mapM_ (vertex.f) pts
                let Just f = suelo (ust st)
                f coo
-}

-----------------------------------------------------------------
-- callbacks
-----------------------------------------------------------------
keyboard st (Char 'p') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}
keyboard _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess

keyboard st (MouseButton WheelUp) _ (Modifiers{shift = Down}) _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {angle = angle (ust s) + 1*degree}}
keyboard st (MouseButton WheelDown) _ (Modifiers{shift = Down}) _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {angle = angle (ust s) -1*degree}}

keyboard st (MouseButton WheelUp) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {zoom = zoom (ust s) *1.2}}
keyboard st (MouseButton WheelDown) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {zoom = zoom (ust s) /1.2}}


keyboard st (SpecialKey KeyRight) Down _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {toshow = toshow (ust s) + 1}}
keyboard st (SpecialKey KeyLeft) Down _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {toshow = max (toshow (ust s) - 1) 0}}

keyboard st (SpecialKey KeyUp) Down _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {basev = basev (ust s) + 1}}
keyboard st (SpecialKey KeyDown) Down _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {basev = max (basev (ust s) - 1) 0}}

keyboard _ _ _ _ _ = return ()

------------------------------------------------------------------

marker st (MouseButton LeftButton) Down _ pos@(Position x y) = do
    modifyIORef st $ \s -> s {ust = (ust s) {marked = map fromIntegral [y,x]: marked (ust s), new = True }}

marker st (MouseButton RightButton) Down _ pos@(Position x y) = do
    modifyIORef st $ \s -> s {ust = (ust s) {marked = tail $ marked (ust s) }}

marker st b s m p = keyboard st b s m p

--------------------------------------------------------------------

easyCamera' fov cen@(cx,cy,cz) pun@(px,py,pz) rho  = 
    CamPar { focalDist = f
           , panAngle = beta
           , tiltAngle = alpha
           , rollAngle = rho+pi
           , cameraCenter = cen
           } where 
    dx = px-cx
    dy = py-cy
    dz = pz-cz
    dh = sqrt (dx*dx+dy*dy)
    f = 1 / tan (fov/2)
    beta = pi+atan2 (-dx) dy
    alpha = atan2 dh (dz)

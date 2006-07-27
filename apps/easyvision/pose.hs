-- some checks

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

data MyState = ST { imgs :: [Img]
                  , marked ::[[Int]]
                  , pts  :: [[Point]]

                  , new    :: Bool
                  , basev ::Int

                  , angle :: Double
                  }

-- | snoc
(=<) :: [a] -> a -> [a]
list =< elem = list ++ [elem]


--------------------------------------------------------------
main = do
    args <- getArgs
    cam@(_,_,(h,w)) <- openCamera (args!!0) Gray (288,384)

    state <- prepare cam ST { imgs=[]
                            , marked = []
                            , pts=[]

                            , new = False

                            , basev = 0

                            , angle = 0
                            }

    addWindow "camera" (w,h) Nothing marker state
    addWindow "selected" (w,h) Nothing keyboard state
    addWindow "3D view" (400,400) Nothing keyboard state

    launch state worker

-------------------------------------------------------------------
a4 = [[   0,    0::Double]
     ,[   0, 2.97]
     ,[2.10, 2.97]
     ,[2.10,    0]
     ]


worker inWindow camera st = do

    let fix = pixel2point camera

    let suaviza = 1 `times` gauss Mask5x5

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
                           imgs = imgs st' =< im
                           }
                else st'

    let st = st''

    inWindow "camera" $ do
        display camera
        mycolor 1 0 0
        drawPixels hotPoints
        mycolor 0 0 1
        drawPixels (marked st)

    let n = length (imgs st)

    when (n > 0) $ do
        let sel = basev st `rem` n
        let ps = pts st !! sel
        let h = estimateHomography ps a4
        let Just pars = poseFromHomogZ0 Nothing h
        let cam = syntheticCamera pars
        let invcam = inverseCamera cam
        inWindow "selected" $ do
            display $ imgs st !! sel
            mycolor 0 1 0
            drawPoints ps
            --print $ panAngle pars / degree
            --print $ tiltAngle pars / degree
            --print $ rollAngle pars / degree
            --print pars

        inWindow "3D view" $ do
            clear [ColorBuffer]
            matrixMode $= Projection
            loadIdentity
            perspective 90 1 1 100
            let ang = angle st
            lookAt (Vertex3 0 (20*sin ang) (20*cos ang)) (Vertex3 0 0 0) (Vector3 0 1 0)
            let f [x,y] = Vertex2 x y
            mycolor 0 0 1
            lineWidth $= 2
            renderPrimitive LineLoop $ mapM_ (vertex.f) a4
            mycolor 1 1 1
            lineWidth $= 1
            let outline = ht invcam (cameraOu 1)
            let f [x,y,z] = Vertex3 x y z
            renderPrimitive LineLoop $ mapM_ (vertex.f) outline

    return st

------------------------------------------------------

compareBy f = (\a b-> compare (f a) (f b))

closest [] p = p
closest hp p = minimumBy (compareBy $ dist p) hp
    where dist [a,b] [x,y] = (a-x)^2+(b-y)^2

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

cameraOu f = 
    [[0::Double,0,0],
    [-1,1,f],
    [1,1,f],
    [1,-1,f],
    [-1,-1,f],
    [-1,1,f],
    [0,0,0],
    [1,1,f],
    [0,0,0],
    [-1,-1,f],
    [0,0,0],
    [1,-1,f],
    [0,0,0],
    [0,0,3*f]
    ]

inverseCamera cam = inv m where
    (k,r,c) = factorizeCamera cam
    m = (r <|> -r <> c) <-> realVector [0,0,0,1]

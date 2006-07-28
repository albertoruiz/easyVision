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
                  , cams :: [Matrix]
                  , drfuns :: [IO()]

                  , new    :: Bool
                  , basev ::Int

                  , angle, angle' :: Double
                  , sccam :: Double
                  }

-- | snoc
(=<) :: [a] -> a -> [a]
list =< elem = list ++ [elem]

diagl = diag . realVector

--------------------------------------------------------------
main = do
    args <- getArgs
    cam@(_,_,(h,w)) <- openCamera (args!!0) Gray (288,384)

    state <- prepare cam ST { imgs=[]
                            , marked = []
                            , pts=[]
                            , cams=[]
                            , drfuns=[]

                            , new = False

                            , basev = 0

                            , angle = 0
                            , angle' = 0
                            , sccam = 1
                            }

    addWindow "camera" (w,h) Nothing marker state
    addWindow "selected" (w,h) Nothing keyboard state
    addWindow "3D view" (400,400) Nothing keyboard state
    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureFunction $= Decal
    launch state worker

-------------------------------------------------------------------
a4 = [[   0,    0::Double]
     ,[   0, 2.97]
     ,[2.10, 2.97]
     ,[2.10,    0]
     ]

recover pts = c where
    h = estimateHomography pts a4
    Just c = cameraFromHomogZ0 Nothing h




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
                    in st { new = False
                          , marked = closest hotPoints n : other
                          }
                else st

    -- when there are four points we add a new view and all required info:
    let newimage = newmark && length (marked st') == 4
    let npts = reverse (fix (marked st'))

    fun <- if newimage
            then genDrawTexture 256 im
            else return (\x->return ())

    let st'' = if newimage
                then st' { pts  = pts  st' =< npts
                         , marked = []
                         , imgs = imgs st' =< im
                         , cams = cams st' =< recover npts
                         , drfuns= drfuns st' =< showcam 1 (recover npts) (Just fun)
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
        inWindow "selected" $ do
            display $ imgs st !! sel
            mycolor 0 1 0
            drawPoints ps

        inWindow "3D view" $ do
            clear [ColorBuffer]
            matrixMode $= Projection
            loadIdentity
            perspective 60 1 1 100
            let ang = angle st
            let ang' = angle' st
            lookAt (Vertex3 (20*sin ang*sin ang') (20*sin ang*cos ang') (20*cos ang)) (Vertex3 0 0 0) (Vector3 0 1 0)
            let f [x,y] = Vertex2 x y
            mycolor 0 0 1
            lineWidth $= 2
            renderPrimitive LineLoop $ mapM_ (vertex.f) a4
            mycolor 1 1 1
            lineWidth $= 1
            sequence_ (drfuns st)

    return st

------------------------------------------------------

compareBy f = (\a b-> compare (f a) (f b))

closest [] p = p
closest hp p = minimumBy (compareBy $ dist p) hp
    where dist [a,b] [x,y] = (a-x)^2+(b-y)^2

showcam size cam Nothing = do
    let (invcam,f) = toCameraSystem cam
    let outline = ht (invcam<>diagl[1,1,1,1/size]) (cameraOutline f)
    let f [x,y,z] = Vertex3 x y z
    renderPrimitive LineLoop $ mapM_ (vertex.f) outline

showcam size cam (Just drfun) = do
    let (invcam,f) = toCameraSystem cam
    let m = invcam<>diagl[1,1,1,1/size]
    let outline = ht m (cameraOutline f)
    let d [x,y,z] = Vertex3 x y z
    drfun $ ht m
              [[ 1,  1, f],
               [-1,  1, f],
               [-1, -1, f],
               [ 1, -1, f]]
    renderPrimitive LineLoop $ mapM_ (vertex.d) outline

-----------------------------------------------------------------
-- callbacks
-----------------------------------------------------------------
keyboard st (Char 'p') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}
keyboard st (Char ' ') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}
keyboard _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess

keyboard st (MouseButton WheelUp) _ (Modifiers{shift = Down}) _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {angle' = angle' (ust s) + 1*degree}}
keyboard st (MouseButton WheelDown) _ (Modifiers{shift = Down}) _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {angle' = angle' (ust s) -1*degree}}

keyboard st (MouseButton WheelUp) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {angle = angle (ust s) + 1*degree}}
keyboard st (MouseButton WheelDown) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {angle = angle (ust s) -1*degree}}

keyboard st (SpecialKey KeyRight) Down _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {sccam = sccam (ust s) + 0.1}}
keyboard st (SpecialKey KeyLeft) Down _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {sccam = max (sccam (ust s) - 0.1) 0.1}}

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

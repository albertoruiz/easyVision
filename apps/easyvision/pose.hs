
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
                  , corners, marked ::[Point]
                  , pts  :: [[Point]]
                  , cams :: [Matrix]
                  , drfuns :: [IO()]

                  , basev ::Int

                  , trackball :: IO ()
                  }

diagl = diag . realVector

--------------------------------------------------------------
main = do
    args <- getArgs
    cam@(_,_,(h,w)) <- openCamera (args!!0) Gray (288,384)

    (tb,kc,mc) <- newTrackball

    state <- prepare cam ST { imgs=[]
                            , corners=[], marked = []
                            , pts=[]
                            , cams=[]
                            , drfuns=[]
                            , basev = 0
                            , trackball = tb
                            }

    addWindow "camera" (w,h) Nothing marker state
    addWindow "selected" (w,h) Nothing keyboard state

    addWindow "3D view" (400,400) Nothing keyboard state
    keyboardMouseCallback $= Just (kc (keyboard state))
    motionCallback $= Just mc
    depthFunc $= Just Less

    textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
    textureFunction $= Decal
    launch state worker

-------------------------------------------------------------------
a4 = [[   0,    0]
     ,[   0, 2.97]
     ,[2.10, 2.97]
     ,[2.10,    0]
     ]

recover pts = c where
    h = estimateHomography pts a4
    Just c = cameraFromHomogZ0 Nothing h

worker inWindow camera st = do
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

    when (n > 0) $ do
        let sel = basev st `rem` n
        let ps = pts st !! sel
        inWindow "selected" $ do
            drawImage $ imgs st !! sel
            pointCoordinates (4,3)
            mycolor 0.75 0 0
            renderPrimitive LineLoop (vertices ps)
            pointSize $= 5
            renderPrimitive Points (vertices ps)

        inWindow "3D view" $ do
            clear [ColorBuffer, DepthBuffer]
            trackball st

            mycolor 0 0 1
            lineWidth $= 2
            renderPrimitive LineLoop (vertices a4)

            mycolor 1 1 1
            lineWidth $= 1
            sequence_ (drfuns st)

    return st {corners = hotPoints}


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
    modifyIORef st $ \s -> s {ust = (ust s) {basev = basev (ust s) + 1}}
keyboard st (SpecialKey KeyDown) Down _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {basev = max (basev (ust s) - 1) 0}}

keyboard _ _ _ _ _ = return ()

------------------------------------------------------------------

marker str (MouseButton LeftButton) Down _ pos@(Position x y) = do
    st <- readIORef str
    let u = ust (st)
    let newpoint = closest (corners u) $ map fromIntegral [x,y]
    let m = newpoint : marked u
    if (length m /= 4)
        then writeIORef str st { ust = u {marked = m} }
        else do
            let hp = reverse $ pixelToPoint (camera st) m
            im  <- scale8u32f 0 1 (camera st)
            let cam = recover hp
            imt <- extractSquare 128 im
            let v = u { marked = []
                       , imgs = imgs u ++ [im]
                       , pts = pts u ++ [hp]
                       , cams = cams u ++ [cam]
                       , drfuns = drfuns u ++ [drawCamera 1 cam (Just imt)]
                       }
            writeIORef str st { ust = v }

marker st (MouseButton RightButton) Down _ pos@(Position x y) = do
    modifyIORef st $ \s -> s {ust = (ust s) {marked = case marked (ust s) of
                                                        [] -> []
                                                        _:t -> t }}

marker st b s m p = keyboard st b s m p


-- This should work with a firewire camera: 
--    $ ./a.out /dev/dv1394
-- or with a raw dv video, for instance:
--    $ wget http://ditec.um.es/~pedroe/svnvideos/misc/penguin.dv
--    $ ./hessian penguin.dv

import Ipp
import Typical
import Draw
import Camera
import Graphics.UI.GLUT
import Data.IORef
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)
import HEasyVision
import Data.List(minimumBy)
import GSL
import Vision

data MyState = ST { smooth :: Int
                  , marked ::[[Int]]
                  , new:: Bool
                  , imgs::[Img]
                  , pts :: [[[Int]]]
                  , toshow :: Int
                  , basev ::Int  
                  }

--------------------------------------------------------------
main = do
    args <- getArgs
    cam@(_,_,(h,w)) <- openCamera (args!!0) Gray (288,384)
    
    state <- prepare cam ST { smooth = 1
                            , marked = []
                            , new=False
                            , imgs=[]
                            , pts=[]
                            , toshow = 0
                            , basev = 0
                            }
    
    addWindow "camera" (w,h) Nothing marker state
    addWindow "hessian" (w,h) Nothing keyboard state
    addWindow "selected" (w,h) Nothing keyboard state
    addWindow "warped" (w,h) Nothing keyboard state
    
    launch state worker
    
-----------------------------------------------------------------
keyboard st (Char 'p') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}
keyboard _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess

keyboard st (MouseButton WheelUp) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {smooth = smooth (ust s) + 1}}
keyboard st (MouseButton WheelDown) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {smooth = max (smooth (ust s) - 1) 0}}

keyboard st (Char '+') Down _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {toshow = toshow (ust s) + 1}}
keyboard st (Char '-') Down _ _ = do
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


-------------------------------------------------------------------

worker inWindow camera st = do

    im  <- scale8u32f 0 1 camera
    img <- (smooth st `times` gauss Mask5x5) im
    h'  <- hessian img
    h  <- scale32f (-1.0) h'
    copyROI32f im h

    inWindow "hessian" $ do
        display im {vroi = vroi h}

    (mn,mx) <- Typical.minmax h
    hotPoints <- localMax 7 h >>= thresholdVal32f (mx/10) 0.0 ippCmpLess >>= getPoints32f 200


    let newmarked = if (new st)
            then let n:other = marked st in closest hotPoints n : other
            else marked st

    let oknew = new st && length (marked st) `rem` 4 == 0

    let newimages = if (oknew)
            then img : imgs st
            else imgs st

    let newpts = if(oknew)
            then take 4 newmarked : pts st
            else pts st


    inWindow "camera" $ do
        display camera
        currentColor $= Color4 1 0 (0::GLfloat) 0
        mydraw hotPoints
        currentColor $= Color4 0 0 (1::GLfloat) 0
        mydraw newmarked

    when (length (imgs st)>0) $ do
        let pdest = (toshow st) `mod` length (imgs st)
        let psrc  = (basev st) `mod` length (imgs st)
        inWindow "selected" $ do
            display (imgs st !! pdest)
            currentColor $= Color4 1 0 (1::GLfloat) 0
            mydraw (pts st !! pdest)
        inWindow "warped" $ do
            let dest = map reverse $ (map (map fromIntegral) $ pts st!!pdest)
            let src  = map reverse $ (map (map fromIntegral) $ pts st!!psrc)
            let t = toList $ estimateHomography dest src
            warp t (imgs st !! psrc) >>= display

    return st {new = False, marked = newmarked, imgs=newimages, pts = newpts}

------------------------------------------------------
mydraw xs = do
    pointSize $= 3
    renderPrimitive Points $ mapM_ f xs where
        f [y,x] = vertex (Vertex2 (fromIntegral x::GLfloat) (288-1-fromIntegral y))
------------------------------------------------------

--compareBy f = (\a b-> compare (f a) (f b))

closest [] p = p
closest hp p = minimumBy (compareBy $ dist p) hp
    where dist [a,b] [x,y] = (a-x)^2+(b-y)^2
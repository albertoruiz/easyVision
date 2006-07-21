-- This should work with a firewire camera: 
--    $ ./a.out /dev/dv1394
-- or with a raw dv video, for instance:
--    $ wget http://ditec.um.es/~pedroe/svnvideos/misc/penguin.dv
--    $ ./hessian penguin.dv

import Ipp hiding (shift)
import Typical
import Draw
import Camera
import Graphics.UI.GLUT hiding (Matrix)
import Data.IORef
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)
import HEasyVision
import Data.List(minimumBy)
import GSL
import Vision
import Autofrontal

data MyState = ST { smooth :: Int
                  , marked ::[[Int]]
                  , new:: Bool
                  , imgs::[Img]
                  , pts :: [[[Double]]]
                  , toshow :: Int
                  , basev ::Int
                  , hs :: [Matrix]
                  , rho :: Double
                  , yh :: Double
                  }

list |> elem = list ++ [elem]

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
                            , hs = []
                            , rho = 0
                            , yh = 2
                            }
    
    addWindow "camera" (w,h) Nothing marker state
    addWindow "hessian" (w,h) Nothing keyboard state
    addWindow "selected" (w,h) Nothing keyboard state
    addWindow "warped" (w,h) Nothing keyboard state
    addWindow "rectified" (w,h) Nothing keyboard state
    
    launch state worker
    
-----------------------------------------------------------------
keyboard st (Char 'p') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}
keyboard _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess

keyboard st (MouseButton WheelUp) _ (Modifiers {shift=Down}) _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {smooth = smooth (ust s) + 1}}
keyboard st (MouseButton WheelDown) _ (Modifiers {shift=Down}) _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {smooth = max (smooth (ust s) - 1) 0}}

keyboard st (MouseButton WheelUp) _ _ _ = do

    modifyIORef st $ \s -> s {ust = (ust s) {yh = yh (ust s) + 0.02}}
keyboard st (MouseButton WheelDown) _ _ _ = do
    modifyIORef st $ \s -> s {ust = (ust s) {yh = yh (ust s) - 0.02}}

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

mycolor r g b = currentColor $= Color4 r g (b::GLfloat) 1
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
            then imgs st |> img
            else imgs st


    let cm = fromIntegral $ width camera `quot` 2
    let rm = fromIntegral $ height camera `quot` 2

    let fix pixs = map f pixs where
        f [r,c] = [x,y] where x  = (fromIntegral c-cm)/cm
                              y  = -(fromIntegral r-rm)/cm

    let nor = desp (-1) (rm/cm) <> diag (realVector [1/cm,-1/cm,1])

    let newpts = if(oknew)
            then pts st |> fix (take 4 newmarked)
            else pts st


    let newhs = if(oknew)
            then genInterimage newpts
            else hs st

    let f = consistency (AllKnown (repeat 2.8)) (tail newhs)

    inWindow "camera" $ do
        display camera
        mycolor 1 0 0
        mydraw hotPoints
        mycolor 0 0 1
        mydraw newmarked

    when (length newimages>0) $ do
        let ptget = (toshow st) `mod` length newimages
        let pbase = (basev  st) `mod` length newimages
        let pt = newpts!!ptget
        let pb = newpts!!pbase
        let ht = newhs!!ptget
        let hb = newhs!!pbase
        let imt = newimages!!ptget
        let imb = newimages!!pbase

        let t = inv hb <> ht
        let t' = inv nor <> t <> nor

        let r0 = inv nor <> (inv $ camera0 ((rho st, yh st),2.8)) <> nor
        let [a,b] = toList $ inHomog $ r0 <> realVector [192,144,1]
        let r = inv nor <> (inv $ camera0 ((rho st, yh st),2.8)) <> ht <> nor
        let r' = scaling 0.2 <> desp (192-a) (144-b) <>r

        inWindow "selected" $ do
            display imb
            mycolor 1 0 1
            mydraw' pb
        inWindow "warped" $ do
            warp (toList t') imt >>= display
        inWindow "rectified" $ do
            warp (toList r') imt >>= display

    when (oknew && length newimages>1 ) $ do
        print(rho st, yh st, f (rho st, yh st))

    
    let [nr,ny] = if True && oknew && length newimages>1
                then fst $ findSol f (0, 1.5)
                else [rho st, yh st]

    return st {new = False, marked = newmarked, imgs=newimages, pts = newpts, hs = newhs,
               rho = nr, yh = ny}

------------------------------------------------------
mydraw xs = do
    pointSize $= 3
    renderPrimitive Points $ mapM_ f xs where
        f [y,x] = vertex (Vertex2 (fromIntegral x::GLfloat) (288-1-fromIntegral y))

mydraw' xs = do
    pointSize $= 3
    renderPrimitive Points $ mapM_ f xs where
        f [x,y] = vertex (Vertex2 x (288-1-y))
------------------------------------------------------

--compareBy f = (\a b-> compare (f a) (f b))

closest [] p = p
closest hp p = minimumBy (compareBy $ dist p) hp
    where dist [a,b] [x,y] = (a-x)^2+(b-y)^2

genInterimage views = map (estimateHomography (head views)) (id views)

camera0 ((rho,yh),f) = reshape 3 $ realVector [
      cr, -ca*sr, -f*sa*sr,
      sr,  ca*cr,  f*cr*sa,
      0,  -sa/f ,  ca ]
    where a = - atan2 f yh
          ca = cos a
          sa = sin a
          cr = cos rho
          sr = - sin rho

scaling s = desp (192) (144) <> 
            realMatrix [[s,0,0],
                        [0,s,0],
                        [0,0,1]] <> desp (-192) (-144)
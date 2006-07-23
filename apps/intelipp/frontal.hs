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

list =< elem = list ++ [elem]

mycolor r g b = currentColor $= Color4 r g (b::GLfloat) 1

encuadra h = desp (-a) (-b) where
    [a,b] = toList $ inHomog $ h <> realVector [0,0,1]

pixel2point (h, w) = (fix, adapt) where
    cm = fromIntegral $ w `quot` 2
    rm = fromIntegral $ h `quot` 2
    nor = desp (-1) (rm/cm) <> diag (realVector [1/cm,-1/cm,1])
    htr = ht :: Matrix -> [[Double]] -> [[Double]]
    fix = htr nor . map (reverse . map fromIntegral)
    adapt h = toList $ inv nor <> h <> nor

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
                            , rho = 30*degree
                            , yh = 5
                            }

    addWindow "camera" (w,h) Nothing marker state
    --addWindow "hessian" (w,h) Nothing keyboard state
    addWindow "selected" (w,h) Nothing keyboard state
    addWindow "warped" (w,h) Nothing keyboard state
    addWindow "rectified" (w,h) Nothing keyboard state
    addWindow "world" (w,h) Nothing keyboard state

    launch state worker

-------------------------------------------------------------------

worker inWindow camera st = do

    let (fix,adapt) = pixel2point (height camera, width camera)
    let suaviza = smooth st `times` gauss Mask5x5

    im  <- scale8u32f 0 1 camera
    h <- suaviza im >>= hessian >>= scale32f (-1.0)

    (mn,mx) <- Typical.minmax h
    hotPoints <- localMax 7 h >>= thresholdVal32f (mx/10) 0.0 ippCmpLess >>= getPoints32f 200


    let newmarked = if (new st)
            then let n:other = marked st in closest hotPoints n : other
            else marked st

    let oknew = new st && length (marked st) `rem` 4 == 0

    let newimages = if (oknew)
            then imgs st =< im
            else imgs st

    let newpts = if(oknew)
            then pts st =< fix (take 4 newmarked)
            else pts st

    let newhs = if(oknew)
            then genInterimage newpts
            else hs st

    when (oknew && length newhs > 1) $ do
        writeFile "real.txt" . show . foldr1 (<->) . tail $ newhs

    --let f = consistency (AllKnown (repeat 2.8)) (tail newhs)
    --let f = consistency (F1Known 2.8) (tail newhs)
    let f = consistency ConstantUnknown (tail newhs)

    inWindow "camera" $ do
        display camera
        mycolor 1 0 0
        mydraw hotPoints
        mycolor 0 0 1
        mydraw newmarked

    when False $ inWindow "hessian" $ do
        auxim <- copy32f im
        copyROI32f auxim h
        display auxim {vroi = vroi h}

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

        --let cam0 = camera0 ((rho st, yh st),2.8)
        let cam0 = fst (extractInfo ConstantUnknown newhs (rho st, yh st))
        let r0 = inv cam0
        let enc = encuadra r0
        let r = scaling 0.2 <> enc <> r0

        inWindow "selected" $ do
            display imb
            --mycolor 1 0 1
            --mydraw' pb
        inWindow "warped" $ do
            warp (adapt t) imt >>= display

        w <- warp (adapt $ r <> ht) imt
        inWindow "rectified" $ do
            display w

        let g im h = warpOn (adapt $ r <> h) w im
        inWindow "world" $ do 
            sequence_ $ zipWith g newimages newhs
            display w

        when (oknew && length newimages>1 ) $ do
            print(rho st, yh st, f (rho st, yh st))
            mapM_ (print . focal . (<> cam0). inv) newhs


    let [nr,ny] = if True && oknew && length newimages>1
                then fst $ findSol f (0, 2)
                else [rho st, yh st]

    when (False && oknew) $ do
        imshow $ environment 100 (20*degree) 0.5 (nr,ny) f

    return st {new = False, marked = newmarked, imgs=newimages, pts = newpts, hs = newhs,
               rho = nr, yh = ny}

------------------------------------------------------
mydraw xs = do
    pointSize $= 3
    renderPrimitive Points $ mapM_ f xs where
        f [y,x] = vertex (Vertex2 (fromIntegral x::GLfloat) (288-1-fromIntegral y))

mydraw' xs = do
    pointSize $= 3
    --ortho2D (-1) (1) (-0.75) (0.75)
    --loadIdentity
    renderPrimitive Points $ mapM_ f xs where
        f [x,y] = vertex (Vertex2 x y)
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

-----------------------------------------------------------------
-- callbacks
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

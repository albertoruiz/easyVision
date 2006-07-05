-- This should work with a firewire camera: 
--    $ ./a.out /dev/dv1394
-- or with a raw dv video, for instance:
--    $ wget http://ditec.um.es/~pedroe/svnvideos/misc/penguin.dv
--    $ ./a.out penguin.dv

import Ipp
import Typical
import Draw
import Camera
import Graphics.UI.GLUT
import Data.IORef
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)

import GSL
import Vision
     

main = do
    args <- getArgs
    cam@(_,_,(h,w)) <- openCamera (args!!0) 1 (288,384)

    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]

    dummy <- img 1 1 100 100
    state <- newIORef State 
                   {camid = cam, frame = 0, pause = False,
                   rawimage = dummy, hess = dummy, locmax = dummy, warped = dummy,
                   smooth = 5, alpha = 0, rho = 0, foc = 600, sca = 1}

    w1 <- installWindow "camera" (w,h) rawimage keyboard state
    w2 <- installWindow "hessian" (w,h) hess keyboard state
    
    attachMenu LeftButton $ Menu [MenuEntry "Quit" (exitWith ExitSuccess)]
    
    w3 <- installWindow "local maxima" (w,h) locmax keyboard state
    w4 <- installWindow "warped" (w,h) warped kbdwarp state

    idleCallback $= Just ( do
        st <- readIORef state
        when (not (pause st)) $ do
            im <- grab (camid st)
            writeIORef state (st {rawimage = im})
        st <- readIORef state    
        newstate <- worker st
        writeIORef state newstate {frame = frame st +1}
        mapM_ (postRedisplay . Just) [w1,w2,w3,w4])

    mainLoop

keyboard st (Char 'p') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}
keyboard _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess
keyboard st (MouseButton WheelUp) _ _ _ = do
    modifyIORef st $ \s -> s {smooth = smooth s + 1}
keyboard st (MouseButton WheelDown) _ _ _ = do
    modifyIORef st $ \s -> s {smooth = max (smooth s - 1) 0}
keyboard _ _ _ _ _ = return ()

------------------------------------------------------

--chg st field op = modifyIORef st $ \s -> s {field = op (field s)}  

kbdwarp st (SpecialKey KeyUp) Down _ _ = do
    modifyIORef st $ \s -> s {alpha = alpha s + 5*degree}        
kbdwarp st (SpecialKey KeyDown) Down _ _ = do
    modifyIORef st $ \s -> s {alpha = alpha s - 5*degree}            

kbdwarp st (SpecialKey KeyRight) _ _ _ = do
    modifyIORef st $ \s -> s {rho = rho s + 5*degree}
kbdwarp st (SpecialKey KeyLeft) _ _ _ = do
    modifyIORef st $ \s -> s {rho = rho s - 5*degree}

kbdwarp st (MouseButton WheelUp) _ _ _ = do
    modifyIORef st $ \s -> s {sca = sca s * 1.1}
kbdwarp st (MouseButton WheelDown) _ _ _ = do
    modifyIORef st $ \s -> s {sca = sca s * 0.9}

kbdwarp st (Char '+') _ _ _ = do
    modifyIORef st $ \s -> s {foc = foc s + 20}
kbdwarp st (Char '-') _ _ _ = do
    modifyIORef st $ \s -> s {foc = max (foc s - 20) 20}


kbdwarp _ _ _ _ _ = return ()
-------------------------------------------------------

data State = 
    State { camid :: Camera
          , frame :: Int 
          , pause :: Bool
          , rawimage :: Img
          , hess :: Img
          , locmax :: Img
          , smooth :: Int
          , warped :: Img
          , alpha, rho, foc, sca :: Double
          }


desp x y = realMatrix [[1,0,x],
                       [0,1,y],
                       [0,0,1]]

scaling s = desp (192) (144) <> 
            realMatrix [[s,0,0],
                        [0,s,0],
                        [0,0,1]] <> desp (-192) (-144)

warper alpha rho foc scale = r where 
    t = desp 192 144 <> kgen foc 
        <> rot1 alpha <> rot3 rho 
        <> kgen (1/foc) <> desp (-192) (-144)
    [a,b] = toList $ inHomog $ t <> realVector [192,144,1]
    r = scaling scale <> desp (192-a) (144-b) <>t

worker st = do
    --when (frame st == 100) (exitWith ExitSuccess)
        
    im  <- scale8u32f 0 1 (rawimage st)
    let t = warper (alpha st) (rho st) (foc st) (sca st)
    w <- warp im (toList t)

    img <- ((smooth st) `times` gauss 55) im
    h   <- hessian img >>= abs32f >>= sqrt32f
    copyROI32f im h
    
    (mn,mx) <- Typical.minmax h
    --print (mn,mx)
    lm <- localMax 7 h >>= thresholdVal32f (mx/2) 0.0 ippCmpLess
    lmr <- imgAs lm
    set32f 0.0 lmr (fullroi lmr)
    copyROI32f lmr lm
    
    return (st {hess = im {vroi = vroi h}, 
                locmax = lmr {vroi = vroi lm},
                warped = w})

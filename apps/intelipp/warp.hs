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
                   rawimage = dummy, warped = dummy,
                   alpha = -40 *degree, rho = 0, foc = 400, sca = 1/2}

    w1 <- installWindow "camera" (w,h) rawimage kbdwarp state
    w2 <- installWindow "warped" (w,h) warped kbdwarp state

    idleCallback $= Just ( do
        st <- readIORef state
        when (not (pause st)) $ do
            im <- grab (camid st)
            writeIORef state (st {rawimage = im})
        st <- readIORef state    
        newstate <- worker st
        writeIORef state newstate {frame = frame st +1}
        mapM_ (postRedisplay . Just) [w1,w2])

    mainLoop

kbdwarp st (Char 'p') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}
kbdwarp _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess

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

    return (st {warped = w})

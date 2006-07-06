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
import HEasyVision
import qualified Data.Map as Map
import Data.Map((!))

import GSL
import Vision
     

main = do
    args <- getArgs
    cam@(_,_,(h,w)) <- openCamera (args!!0) Gray (288,384)

    state <- prepare cam $ Map.fromList 
                             [("alpha",-40*degree) 
                             ,("rho",0) 
                             ,("foc" , 400)
                             ,("sca",1/2)]

    addWindow "camera" (w,h) Nothing kbdwarp state
    addWindow "warped" (w,h) Nothing kbdwarp state

    launch state worker
    
----------------------------------------------------------------------    
modify st str fun = do
    modifyIORef st $ \s -> s {ust = Map.insertWith g str 0 (ust s)} 
       where g a b = fun b

kbdwarp st (Char 'p') Down _ _ = do
    modifyIORef st $ \s -> s {pause = not (pause s)}
kbdwarp _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess

kbdwarp st (SpecialKey KeyUp) Down _ _ = modify st "alpha" (+5*degree)
kbdwarp st (SpecialKey KeyDown) Down _ _ = modify st "alpha" (+ (-5*degree))

kbdwarp st (SpecialKey KeyRight) Down _ _ = modify st "rho" (+5*degree)
kbdwarp st (SpecialKey KeyLeft) Down _ _ = modify st "rho" (+ (-5*degree))

kbdwarp st (MouseButton WheelUp) _ _ _ = modify st "sca" (*1.1)
kbdwarp st (MouseButton WheelDown) _ _ _ = modify st "sca" (*0.9)

kbdwarp st (Char '+') _ _ _ = modify st "foc" (+20)
kbdwarp st (Char '-') _ _ _ = modify st "foc" $ max 20 . (+(-20)) 

kbdwarp _ _ _ _ _ = return ()
-------------------------------------------------------


desp x y = realMatrix [[1,0,x],
                       [0,1,y],
                       [0,0,1]]

scaling s = desp (192) (144) <> 
            realMatrix [[s,0,0],
                        [0,s,0],
                        [0,0,1]] <> desp (-192) (-144)

warper alpha rho foc sca = toList r where 
    t = desp 192 144 <> kgen foc 
        <> rot1 alpha <> rot3 rho 
        <> kgen (1/foc) <> desp (-192) (-144)
    [a,b] = toList $ inHomog $ t <> realVector [192,144,1]
    r = scaling sca <> desp (192-a) (144-b) <>t

----------------------------------------------------------

worker inWindow camera param = do
        
    inWindow "camera" (display camera)
    
    let t = warper (param!"alpha") (param!"rho") (param!"foc") (param!"sca")
    inWindow "warped" $ do
        scale8u32f 0 1 camera >>= warp t >>= display

    return param

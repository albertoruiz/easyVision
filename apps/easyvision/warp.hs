-- This should work with a firewire camera: 
--    $ ./a.out /dev/dv1394
-- or with a raw dv video, for instance:
--    $ wget http://ditec.um.es/~pedroe/svnvideos/misc/penguin.dv
--    $ ./a.out penguin.dv

import Ipp
import Graphics.UI.GLUT hiding (Size)
import Data.IORef
import System.Exit
import Control.Monad(when)
import System.Environment(getArgs)
import qualified Data.Map as Map
import Data.Map((!))

import GSL
import Vision

szc = Size 288 384
szw = Size 600 500

main = do
    args <- getArgs
    let sz = Size 288 384
    cam <- openCamera (args!!0) szc

    state <- prepare cam $ Map.fromList 
                             [("alpha",-40*degree) 
                             ,("rho",0) 
                             ,("foc" , 2)
                             ,("sca",1/2)]

    addWindow "camera" szc Nothing kbdwarp state
    addWindow "warped" szw Nothing kbdwarp state

    launch state worker

----------------------------------------------------------------------    
modify st str fun = do
    modifyIORef st $ \s -> s {ust = Map.insertWith g str 0 (ust s)} 
       where g a b = fun b

kbdwarp str (Char 'p') Down _ _ = do
    st <- readIORef str
    pause (camid st)
kbdwarp str (Char ' ') Down _ _ = do
    st <- readIORef str
    pause (camid st)
kbdwarp _ (Char '\27') Down _ _ = do
    exitWith ExitSuccess

kbdwarp st (SpecialKey KeyUp) Down _ _ = modify st "alpha" (+5*degree)
kbdwarp st (SpecialKey KeyDown) Down _ _ = modify st "alpha" (+ (-5*degree))

kbdwarp st (SpecialKey KeyRight) Down _ _ = modify st "rho" (+5*degree)
kbdwarp st (SpecialKey KeyLeft) Down _ _ = modify st "rho" (+ (-5*degree))

kbdwarp st (MouseButton WheelUp) _ _ _ = modify st "sca" (*1.1)
kbdwarp st (MouseButton WheelDown) _ _ _ = modify st "sca" (*0.9)

kbdwarp st (Char '+') _ _ _ = modify st "foc" (*1.2)
kbdwarp st (Char '-') _ _ _ = modify st "foc" (/1.2)

kbdwarp _ _ _ _ _ = return ()
-------------------------------------------------------

vector v = fromList v :: Vector Double

warper alpha rho foc sca = r where 
    t = kgen foc
        <> rot1 alpha <> rot3 rho 
        <> kgen (1/foc)
    [a,b] = toList $ inHomog $ t <> vector [0,0,1]
    r = scaling sca <> desp (-a,-b) <> t

----------------------------------------------------------

worker inWindow cam param = do

    camera <- grab cam
    inWindow "camera" (drawImage camera)

    let t = warper (param!"alpha") (param!"rho") (param!"foc") (param!"sca")
    inWindow "warped" $ do
        scale8u32f 0 1 camera >>= warp szw t >>= drawImage

    return param

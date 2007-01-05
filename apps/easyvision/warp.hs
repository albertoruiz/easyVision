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

    state <- prepare cam ()

    param <- createParameters state [("alpha", realParam (-40) (-100) (100))
                                     ,("rho",  realParam 0 (-180) (180))
                                     ,("foc",  listParam 2 [0.5, 0.7, 1, 2, 5,5.5, 9,10])
                                     ,("sca",  listParam 0.5 [1.1**k|k<-[-20..20]])]

    addWindow "camera" szc Nothing kbdwarp state
    addWindow "warped" szw Nothing kbdwarp state

    launch state (worker param)

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

worker param inWindow cam st = do

    camera <- grab cam
    inWindow "camera" (drawImage camera)

    alpha <- getParam param "alpha"
    rho   <- getParam param "rho"
    foc   <- getParam param "foc"
    sca   <- getParam param "sca"

    let t = warper (alpha*degree) (rho*degree) foc sca
    inWindow "warped" $ do
        scale8u32f 0 1 camera >>= warp szw t >>= drawImage

    return st

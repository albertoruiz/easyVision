-- sequential vs pipeline. Compare:
-- time ./pipeline 'video.avi -benchmark' --guifps=100 --sizework=40 --seq
-- time ./pipeline 'video.avi -benchmark' --guifps=100 --sizework=40

import EasyVision
import Graphics.UI.GLUT(postRedisplay)
import Control.Monad(liftM2)
import Control.Arrow
import Data.IORef
import System.Exit

monitor' name sz fun = do
    w <- evWindow () name sz (Just (const fun)) (const kbdQuit)
    return $ postRedisplay (Just (evW w))

observe' n f = observe'' n $ uncurry (>>) . ((drawImage.f) *** shFrame)
observe'' winname f a = monitor' winname (mpSize 20) (a >>= f)

runGUI n ws = sequence ws >>= launchFreq n . sequence_

camera = findSize >>= getCam 0 ~> channels
observe winname f = monitor winname (mpSize 20) (drawImage.f)
run c = prepare >> (c >>= launch . (>> return ()))

counter tot cam = do
    vn <- newIORef tot
    return $ do
        n <- readIORef vn
        if n==0 then exitWith ExitSuccess else writeIORef vn (n-1)
        cam

async f = asyncFun 0 id f

syncFun' f n = syncFun (f***id) n

infixl 1 -<
(f,d) -< n = asyncFun d f n
--(.&.) = liftM2 (,)

edges th img = canny (gx,gy) (th*2/3,th) where
    gx = (-1) .* sobelVert img
    gy =         sobelHoriz img

fun1 sigma thres szw = edges thres . gaussS sigma . float. resize (mpSize szw). gray

fun2 = distanceTransform [1,1.4,2.2] . notI

---------------------------------------------------------------------

mainPipe = do
    sigma <- getOption "--sigma" 3
    thres <- getOption "--thres" 0.1
    szw   <- getOption "--sizework" 40

    prepare

    (cam,vcam) <- mkWatch $ camera ~~> flip zip [(0::Int)..]
    ([e], ve)  <- syncFun' (fun1 sigma thres szw) 1 cam
    ([dt],_)   <- syncFun' fun2 1 e

    getOption "--guifps" 30 >>= flip runGUI
           [ observe' "cam" gray vcam
           , observe' "eges"    id ve
           , observe' "disttrans" ((1/60) .*)  dt
           , counter 500 (return ())
           ]

shFrame n = do
    setColor 1 1 1
    text2D 20 20 ("frame="++show n)

--------------------------------------------------------------------------------

mainSeq = do
    sigma <- getOption "--sigma" 3
    thres <- getOption "--thres" 0.1
    szw   <- getOption "--sizework" 40

    run    $   camera >>= counter 500
           >>= observe "original" gray
           ~>  fun1 sigma thres szw
           >>= observe "edges" id
           ~>  fun2
           >>= observe "distTrans" (((1/60) .*))

--------------------------------------------------------------------------------

main = do
    sq <- getFlag "--seq"
    if sq then mainSeq else mainPipe

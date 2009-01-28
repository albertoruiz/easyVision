-- sequential vs pipeline. Compare:
-- time ./pipeline 'video.avi -benchmark -frames 100' --guifps=100 --sizework=35 --seq  +RTS  -N2
-- time ./pipeline 'video.avi -benchmark -frames 100' --guifps=100 --sizework=35        +RTS  -N2

import EasyVision
import Graphics.UI.GLUT(postRedisplay)
import Control.Monad(liftM2)
import Control.Arrow

monitor' name sz fun = do
    w <- evWindow () name sz (Just (const fun)) (const kbdQuit)
    return $ postRedisplay (Just (evW w))

observe' n f = observe'' n $ uncurry (>>) . ((drawImage.f) *** shFrame)
observe'' winname f a = monitor' winname (mpSize 20) (a >>= f)

runGUI n ws = sequence ws >>= launchFreq n . sequence_

camera = findSize >>= getCam 0 ~> channels
observe winname f = monitor winname (mpSize 20) (drawImage.f)
run c = prepare >> (c >>= launch . (>> return ()))

async f = asyncFun 0 id f

syncFun' f n = syncFun (f***id) n

infixl 1 -<
(f,d) -< n = asyncFun d f n
--(.&.) = liftM2 (,)

edges th img = canny (gx,gy) (th*2/3,th) where
    gx = (-1) .* sobelVert img
    gy =         sobelHoriz img

--fun1 sigma thres szw = edges thres . gaussS sigma . float. resize (mpSize szw). gray

--fun2 = distanceTransform [1,1.4,2.2] . notI

fun1 _ _ szw = gaussS 20 . float. resize (mpSize szw). gray
fun2 = gaussS 20 . gaussS 20

---------------------------------------------------------------------

mainSync = do
    sigma <- getOption "--sigma" 3
    thres <- getOption "--thres" 0.1
    szw   <- getOption "--sizework" 40

    prepare

    (cam,vcam) <- mkWatch $ camera ~~> flip zip [(0::Int)..]
    ([e], ve)  <- syncFun' (fun1 sigma thres szw) 1 cam
    ([dt],_)   <- syncFun' fun2 1 e

    getOption "--guifps" 30 >>= flip runGUI
           [ observe' "cam" gray vcam
           , observe' "f1"    id ve
           , observe' "f2"    id dt
           ]

shFrame n = do
    setColor 1 1 1
    text2D 20 20 ("frame="++show n)

--------------------------------------------------------------------------------

mainSeq = do
    sigma <- getOption "--sigma" 3
    thres <- getOption "--thres" 0.1
    szw   <- getOption "--sizework" 40

    run    $   camera
           >>= observe "cam" gray
           ~>  fun1 sigma thres szw
           >>= observe "f1" id
           ~>  fun2
           >>= observe "f2" id

----------------------------------------------------------------------------

mainPipe = do
    sigma <- getOption "--sigma" 3
    thres <- getOption "--thres" 0.1
    szw   <- getOption "--sizework" 40

    putStrLn "auto pipeline"

    run    $   camera
           >>= observe "cam" gray
           ~>  fun1 sigma thres szw
--           >>= observe "f1" id
           ~~> pipeline fun2
           >>= observe "f2" id

--------------------------------------------------------------------------------

main = do
    sq  <- getFlag "--seq"
    syn <- getFlag "--sync"
    if sq then mainSeq else if syn then mainSync else mainPipe

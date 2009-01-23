import EasyVision
import Graphics.UI.GLUT(postRedisplay)
import Control.Monad(liftM2)

monitor' name sz fun = do
    w <- evWindow () name sz (Just (const fun)) (const kbdQuit)
    return $ postRedisplay (Just (evW w))

observe winname f a = monitor' winname (mpSize 20) (a >>= drawImage.f)

run n ws = sequence ws >>= launchFreq n . sequence_

async f = asyncFun 0 id f

infixl 1 -<
(f,d) -< n = asyncFun d f n
(.&.) = liftM2 (,)

main = do
    prepare

    (cam,vcam) <- mkWatch $ findSize >>= getCam 0 ~> channels

    ([x1,x2], vx)   <- syncFun (float.gray) 2 cam
    ([a],_)         <- syncFun (gaussS 5) 1 x1
    ([b1],vb1)      <- syncFun (gaussS 4) 1 x2
    ([b],_)         <- syncFun (gaussS 3) 1 b1
    d               <- asyncFun 0 (\(u,v) -> 100.*(u |-|v)) ( a .&. b )

    run 20 [ observe "cam" rgb vcam
           , observe "dif"   id d
           , observe "b1"  id vb1
           ]

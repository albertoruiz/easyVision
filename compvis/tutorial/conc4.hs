import EasyVision
import Graphics.UI.GLUT(postRedisplay)
import Control.Monad(liftM2)
import Control.Arrow

monitor' name sz fun = do
    w <- evWindow () name sz (Just (const fun)) (const kbdQuit)
    return $ postRedisplay (Just (evW w))

observe winname f a = monitor' winname (mpSize 20) (a >>= f)

run n ws = sequence ws >>= launchFreq n . sequence_

async f = asyncFun 0 id f

infixl 1 -<
f -< n = asyncFun (hz 30) (f***id) n
(.&.) = liftM2 (,)
hz d = 10^6 `div` d

main = do
    prepare

    (cam,vcam) <- mkWatch $ findSize >>= getCam 0 ~> channels ~~> flip zip [(0::Int)..]

    x               <- float.gray -< cam
    a               <- gaussS 5   -< x
    b1              <- gaussS 4   -< x
    b               <- gaussS 3   -< b1
    d               <- asyncFun 0 dif ( a .&. b )

    run 20 [ observe' "cam" rgb vcam
           , observe' "dif"   id d
           , observe' "b1"  id b
           ]

dif ((u,n1) ,(v,n2)) = (100.*(u |-|v),(n1-n2))

shFrame n = do
    setColor 1 1 1
    text2D 20 20 ("frame="++show n)

observe' n f = observe n $ uncurry (>>) . ((drawImage.f) *** shFrame)

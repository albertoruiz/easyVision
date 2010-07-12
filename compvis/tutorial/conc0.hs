import EasyVision hiding (run,observe,(.&.))
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

hz d = 10^6 `div` d

main = do
    prepare

    cam <- findSize >>= getCam 0 >>= async ~> channels
    x <- (float . gray , hz 2) -< cam
    s <- (float.highPass8u Mask5x5 . gray , hz 30) -< cam
    dif <- (\(u,v) -> (0.8 .* u |+| 0.2 .* v), hz 25) -< x .&. s

    run 20 [ observe "cam" rgb cam
           , observe "dif" id dif
           , observe "s" id s
           , observe "x" id x
           ]

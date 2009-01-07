import EasyVision
import Graphics.UI.GLUT

camera = prepare >> findSize >>= getCam 0 ~> float . gray . channels
observe winname f = monitor winname (mpSize 20) f
run = launch . (>> return ())

data Param = Param { sigma :: Float, rad :: Int, thres :: Float }

main =   camera >>= userParam
     ~>  wi corners
     >>= observe "corners" sh >>= run

corners (x,p) = getPoints32f 100
              . localMax (rad p)
              . thresholdVal32f (thres p) 0 IppCmpLess
              . fixscale
              . hessian
              . gradients
              . gaussS (sigma p)
              $ x

wi f x = (x, f x)

fixscale im = (1/mn) .* im
    where (mn,_) = EasyVision.minmax im

sh ((im,_), pts) = do
    drawImage im
    pointSize $= 5; setColor 1 0 0
    renderPrimitive Points $ mapM_ vertex pts

userParam cam = do
    o <- createParameters [("sigma",realParam 3 0 20),
                           ("rad"  ,intParam  4 1 25),
                           ("thres",realParam 0.6 0 1)]
    return $ do
        x <- cam
        s <- getParam o "sigma"
        r <- getParam o "rad"
        t <- getParam o "thres"
        return (x, Param {sigma = s, rad = r, thres = t})

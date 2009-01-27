import EasyVision
import Graphics.UI.GLUT
import Control.Arrow
import Control.Monad

camera = findSize >>= getCam 0 ~> (channels >>> gray >>> float)
observe winname f = monitor winname (mpSize 20) f
run c = prepare >> (c >>= launch . (>> return ()))
(.&.) = liftM2 (liftM2 (,))

data Param = Param { sigma :: Float, rad :: Int, thres :: Float }

main = run $ camera .&. userParam
         ~>  fst &&& corners
         >>= observe "corners" sh

corners (x,p) =  gaussS (sigma p)
             >>> gradients
             >>> hessian
             >>> fixscale
             >>> thresholdVal32f (thres p) 0 IppCmpLess
             >>> localMax (rad p)
             >>> getPoints32f 100
              $  x

fixscale im = (1/mn) .* im
    where (mn,_) = EasyVision.minmax im

sh (im, pts) = do
    drawImage im
    pointSize $= 5; setColor 1 0 0
    renderPrimitive Points $ mapM_ vertex pts

userParam = do
    o <- createParameters [("sigma",realParam 3 0 20),
                           ("rad"  ,intParam  4 1 25),
                           ("thres",realParam 0.6 0 1)]
    return $ return Param `ap` getParam o "sigma"
                          `ap` getParam o "rad"
                          `ap` getParam o "thres"

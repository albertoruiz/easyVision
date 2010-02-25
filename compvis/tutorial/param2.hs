import EasyVision
import Graphics.UI.GLUT
import Control.Arrow
import Control.Monad

(.&.) = liftM2 (liftM2 (,))

camera' = camera ~> (gray >>> float)

data Param = Param { sigma :: Float, rad :: Int, thres :: Float }

main = run $ camera' .&. userParam
         ~>  fst &&& corners
         >>= monitor "corners" (mpSize 20) sh

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

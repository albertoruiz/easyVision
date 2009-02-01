import EasyVision
import Graphics.UI.GLUT
import Control.Arrow
import Debug.Trace

-------------------------------

salience s1 s2 = gaussS' 2.5 s2 . sqrt32f . abs32f . hessian . gradients . gaussS' 2.5 s1

-------------------------------

camera = findSize >>= getCam 0 ~> gray . channels
observe winname f = monitor winname (mpSize 20) f
run c = prepare >> (c >>= launch . (>> return ()))

save filename f cam = do
    writeFile filename ""
    return $ do
        x <- cam
        appendFile filename (show (f x)++"\n")
        return x

-----------------------------------------------------------------------------------------------

sh (im, pts) = do
    drawImage im
    pointSize $= 5; setColor 1 0 0
    renderPrimitive Points $ mapM_ vertex pts

thres r im = thresholdVal32f (mx*r) 0 IppCmpLess im
    where (_,mx) = EasyVision.minmax im

main = run $   camera
           ~>  id &&& (getPoints32f 300 . localMax 1 . thres 0.5 . salience 2 4 . float)
           >>= observe "filter" sh
           >>= save "points.txt" snd

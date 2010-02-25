import EasyVision
import Graphics.UI.GLUT
import Control.Arrow

save filename f cam = do
    writeFile filename ""
    return $ do
        x <- cam
        appendFile filename (show (f x)++"\n")
        return x

sh (im, pts) = do
    drawImage im
    pointSize $= 5; setColor 1 0 0
    renderPrimitive Points $ mapM_ vertex pts

salience s1 s2 = gaussS' 2.5 s2 . sqrt32f . abs32f . hessian . gradients . gaussS' 2.5 s1

thres r im = thresholdVal32f (mx*r) 0 IppCmpLess im
    where (_,mx) = EasyVision.minmax im

main = run $   camera ~> gray
           ~>  id &&& (getPoints32f 300 . localMax 1 . thres 0.5 . salience 2 4 . float)
           >>= monitor "Corners" (mpSize 20) sh
           >>= save "points.txt" snd

import Vision.GUI
import ImagProc

main = run  $    arr (grayscale >>> id &&& corners)
            >>>  observe "Corners" sh

sh (im, pts) = [ Draw im, pointSz 5, color red, drw pts ]
  where
    drw = Draw . points . pixelsToPoints (size im) 

salience s1 s2 = gaussS s2 . sqrt32f . abs32f . hessian . gradients . gaussS s1

corners = getPoints32f 300 . localMax 1 . thres 0.5 . salience 2 4 . float

thres r im = thresholdVal32f (mx*r) 0 IppCmpLess im
    where (_,mx) = minmax im


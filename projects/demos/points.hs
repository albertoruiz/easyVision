import Vision.GUI
import ImagProc

main = run  $    arr (grayscale >>> id &&& interest)
            >>>  observe "Corners" sh

sh (im, pts) = [ Draw im, pointSz 5 . color red $ pts ]

interest :: ImageGray -> [Point]
interest g = pixelsToPoints (size g) . getPoints32f 300 . localMax 1 . thres 0.5 . salience 2 4 . float $ g
  where
    thres r im = thresholdVal32f (mx*r) 0 IppCmpLess im
        where (_,mx) = minmax im
    salience s1 s2 = gaussS s2 . sqrt32f . abs32f . hessian . gradients . gaussS s1


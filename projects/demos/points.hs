import Vision.GUI
import Image.Processing
import Image.Devel
import Image.Processing.Custom(getPoints32f)

main = run  $    arr grayf
            >>> observe "salience" (salience 2 4)
            >>> observe "salienceC" ((10 .*) . salienceC 4)
            >>>  arr (id &&& interest)
           >>>  observe "Corners" sh

sh (im, pts) = [ Draw im, pointSz 5 . color red $ pts ]

interest :: Image Float -> [Point]
interest g  =  pixelsToPoints (size g) . getPoints32f 300 . localMax 1
            .  thres 0.2 . salienceC 4 $ g

thres r im = thresholdVal (mx*r) 0 IppCmpLess im
    where (_,mx) = minmax im

salience s1 s2 = gaussS s2 . sqrt32f . abs32f . hessian . gradients . gaussS s1

eqmax x = compareImages IppCmpEq x (filterMax 1 x)

salienceC s1 = {-sqrt32f . thres 0.01 . -} ((-1) .*) . hessian . gradients . gaussS s1


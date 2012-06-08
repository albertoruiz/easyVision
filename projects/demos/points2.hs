import Vision.GUI
import ImagProc

main = run  $ arr grayscale >>>  sMonitor "Corners" sh

sh _ im = [ Draw [ Draw im, pointSz 5 . color red $ interest im ]
          , Draw [ Draw im, pointSz 5 . color blue $ corners im  ]
          ]


interest :: ImageGray -> [Point]
interest g = pixelsToPoints (size g) . getPoints32f 300 . localMax 1 . thres 0.5 . salience 2 4 . float $ g
  where
    thres r im = thresholdVal32f (mx*r) 0 IppCmpLess im
        where (_,mx) = minmax im
    salience s1 s2 = gaussS s2 . sqrt32f . abs32f . hessian . gradients . gaussS s1


corners :: ImageGray -> [Point]
corners g = pixelsToPoints (size g) . getPoints32f 300 . localMax 1 . salience 1 1 . float $ g
  where
    thres r im = thresholdVal32f (mx*r) 0 IppCmpLess im
        where (_,mx) = minmax im
    salience s1 s2 =  thres 0.5 . gaussS s2 . sqrt32f . thres 0.1 . ((-1) .*) . hessian . gradients . gaussS s1


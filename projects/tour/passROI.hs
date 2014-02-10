import Vision.GUI
import Image.Processing
import Image.ROI


main = run $ arr grayscale >>> getROI "change roi" >>> arr cleanROI >>> observe "only roi" Draw

cleanROI im = resize (roiSize (roi im)) im

getROI name = transUI
            $ interface (Size 240 360) name state0 firsttime updts acts result display
  where
    state0        = ()
    firsttime _ _ = return ()
    updts         = []
    acts          = []
    result droi _s input = ((), setRegion droi input)
    display _droi _s _input output = Draw output


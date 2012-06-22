import Vision.GUI
import ImagProc
import Contours.Base(setRegion)

main = run $ arr grayscale >>> getROI "change roi" >>> arr cleanROI >>> observe "only roi" Draw

cleanROI im = resize (roiSize (theROI im)) im

getROI name = transUI
            $ interface (Size 240 360) name state0 firsttime updts acts result display
  where
    state0        = ()
    firsttime _ _ = return ()
    updts         = []
    acts          = []
    result roi _s input = ((), setRegion roi input)
    display _roi _s _input output = Draw output


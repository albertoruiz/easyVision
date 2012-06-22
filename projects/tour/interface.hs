import Vision.GUI hiding (clickPoints)
import ImagProc

main = run clickPoints

clickPoints :: ITrans Channels ([Point], ImageGray)
clickPoints = transUI $ interface  (Size 240 320) "click points"
                                   state0 firsttime updts acts result display
  where
    state0 = []
    firsttime _ _ = return ()
    updts = [(key (MouseButton LeftButton), \_roi pt pts -> pt:pts)]
    acts  = []
    result _roi pts input = (pts, (pts, notI . grayscale $ input))
    display _roi _pts _input (pts,x) = Draw  [ Draw x, drwpts ]
      where drwpts = (color green . pointSz 3) pts


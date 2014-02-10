import Vision.GUI hiding (clickPoints)
import Image.Processing

main = run clickPoints

clickPoints :: ITrans Channels ([Point], Image Gray)
clickPoints = transUI $ interface  (Size 240 320) "click points"
                                   state0 firsttime updts acts result display
  where
    state0 = []
    firsttime _ _ = return ()
    updts = [(key (MouseButton LeftButton), \_droi pt pts -> pt:pts)]
    acts  = []
    result _droi pts input = (pts, (pts, notI . grayscale $ input))
    display _droi _pts _input (pts,x) = Draw  [ Draw x, drwpts ]
      where drwpts = (color green . pointSz 3) pts


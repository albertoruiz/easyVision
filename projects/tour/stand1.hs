import Vision.GUI
import ImagProc

main = runIt win

win = standalone (Size 100 400) "click to change" x0 updts [] sh
  where
    x0 = 7
    sh = text (Point 0 0) . show
    updts = [(key (MouseButton LeftButton), \roi pt -> (+1)) ]


import Vision.GUI
import Image

main = runIt win

win = standalone (Size 100 400) "click to change" x0 updts [] sh
  where
    x0 = 7
    sh = text (Point 0 0) . show
    updts = [(key (MouseButton LeftButton), \_droi _pt -> (+1)) ]


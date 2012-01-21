import Vision.GUI
import ImagProc

main = runIt win

win = browser "odd numbers" xs sh
  where
    xs = [1,3 .. 21]
    sh _k = text (Point 0 0) . show


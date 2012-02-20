import Vision.GUI
import ImagProc
import Contours
import Vision
import Contours.Clipping

main = runIt win

win = browser "clipping" xys sh
    where
      cs = map (pre) pentominos
      xys = zip cs (tail cs)
      sh k (a,b) = Draw [ color yellow zs , color blue ys ]
        where
          zs = clip ClipDifference a b
          ys = clip ClipDifference b a
      
pre = transPol (scaling 0.2) . whitenContour . fst


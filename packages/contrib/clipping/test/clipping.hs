import Vision.GUI
import ImagProc
import Contours
import Vision
import Contours.Clipping
import Util.Misc(debug)

main = runIt win

win = browser "clipping" xys sh
    where
      cs = map (whitenContour.fst) pentominos
      xys = zip cs (tail cs)
      sh k (a,b) = Draw [ color red, dr a, color blue, dr b, color yellow, Draw (map dr zs)]
        where
          zs = clip ClipIntersection a b
      
dr = Draw . transPol (scaling 0.2)


import Vision.GUI
import ImagProc
import Contours
import Vision
import Contours.Clipping
import Util.Misc(debug)
import Text.Printf(printf)

main = runIt win

win = browser "clipping" xys sh
    where
      cs = map (pre) pentominos
      xys = zip cs (tail cs)
      sh k (a,b) = Draw [ color red a, color blue b, color yellow zs
                        , color white $ map drp (a:b:zs) 
                        ]
        where
          zs = clip ClipIntersection a b

pre = transPol (scaling 0.2) . whitenContour . fst

drp ps = Draw $ map g (polyPts ps)
  where
    g p@(Point x y) = textF Helvetica10 p (printf "(%.2f,  %.2f)" x y)


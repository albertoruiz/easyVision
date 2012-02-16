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
      cs = map (pre.whitenContour.fst) pentominos
      xys = zip cs (tail cs)
      sh k (a,b) = Draw [ color yellow, Draw (map dr zs), color blue, Draw (map dr ys),
                          Draw (map drp (a:b:zs))]
        where
          zs = clip ClipDifference a b
	  ys = clip ClipDifference b a
      
pre = transPol (scaling 0.2)

dr = Draw

drp ps = Draw [color white,  Draw $ map g (polyPts ps) ]
  where
    g p@(Point x y) = textF Helvetica10 p (printf "(%.2f,  %.2f)" x y)


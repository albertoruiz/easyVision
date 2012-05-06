import Vision.GUI
import ImagProc
import Contours
import Vision
import Contours.Clipping

main = runIt $ do
    win "union"        ClipUnion
    win "intersection" ClipIntersection
    win "difference"   ClipDifference
    win "difference2"  ClipDifferenceFlip

win name mode = browser name xys sh
    where
      cs = map (pre) pentominos
      xys = zip cs (tail cs)
      sh k (a,b) = clearColor white 
                        [ lineWd 7 [ color lightblue a, color pink b ]
                        , lineWd 2 [ color black zs ]
                        ]
        where
          zs = clip mode a b
      
pre = transPol (scaling 0.2) . whitenContour . fst


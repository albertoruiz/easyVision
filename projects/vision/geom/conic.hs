import Vision.GUI.Simple
import Image
import Contours(asSegments)
import Util.Ellipses
import Util.Geometry

main = runIt $ clickPoints "conic" "--points" () (sh.fst)

l = gjoin (Point 0.5 0) (Point 0 0.5)

sh pts | length pts >= 5 = Draw 
            [ drwpts
            , (color col . drawConic) c
            , color red l
            , color yellow . pointSz 3 $ intersectionConicLine c l
            ]
       | otherwise = drwpts
  where
    c = computeConic pts
    drwpts = color white . drawPointsLabeled $ pts
    col = if isEllipse c then green else orange
    isEllipse c = null (intersectionConicLine c linf)
    linf = HLine 0 0 1

drawConic c = Draw ss
  where
    ps = pointsConic 50 c
    ss = filter ((1>).segmentLength) $ asSegments (Closed ps)


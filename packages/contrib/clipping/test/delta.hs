import Vision.GUI
import ImagProc
import Contours
import Vision
import Contours.Clipping
import Data.List(partition)
import Util.Misc(debug)

main = runIt win

win = browser "clipping" ds (const id)
    where
      ds = [ msg "original contours" [color blue a, color red b]
           , msg "XOR" [color blue (map (fst.fst) zp), color red (map (fst.fst) zn)]
           , msg "delta" (drawDelta deltas)
           , msg "delta" (drawDelta (deltaContour a b))
           ]
   --        ++ map (msg "positive" . color blue . shOrig) zp
   --        ++ map (msg "negative" . color red  . shOrig) zn


a' = Closed [Point x1 y1, Point 0 y1, Point x2 y1, Point x2 y2, Point x1 y2]
  where
    x1 = 0.6; x2 = -x1; y1 = 0.4; y2 = -y1

b' = Closed [ Point x1 y2,  Point x1 0, Point x1 y1, Point x2 y1, Point x2 y2
           , Point x4 y2, Point x4 y3, Point x3 y3, Point x3 y2]
  where
    x1 = 0.4; x2 = -x1; x3 = 0.2; x4 = -x3;
    y1 = 0.8; y2 = -0.6; y3 = 0.6


a = Closed [Point x1 y0, Point x8 y0, Point x8 y7, Point x6 y7, Point x6 y1
           , Point x4 y1, Point x4 y7, Point x3 y7, Point x3 y1, Point x2 y1
           , Point x2 y7, Point x1 y7 ]
  where
    x0 = 0.9; x1 = 0.8; x2 = 0.4; x3 = 0.2; x4 =  0.0; x5 = -0.2; x6 = -0.4; x7 = -0.6; x8 = -0.8; x9 = -0.9;
    y0 = 0.8; y1 = 0.4; y2 = 0.2; y3 = 0.0; y4 = -0.2; y5 = -0.4; y6 = -0.6; y7 = -0.8;

b = Closed [Point x0 y2, Point x9 y2, Point x9 y6, Point x0 y6, Point x0 y5 
           , Point x7 y5, Point x7 y4, Point x5 y4, Point x5 y3, Point x0 y3]
  where
    x0 = 0.9; x1 = 0.8; x2 = 0.4; x3 = 0.2; x4 =  0.0; x5 = -0.2; x6 = -0.4; x7 = -0.6; x8 = -0.8; x9 = -0.9;
    y0 = 0.8; y1 = 0.4; y2 = 0.2; y3 = 0.0; y4 = -0.2; y5 = -0.4; y6 = -0.6; y7 = -0.8;


deltas = deltaContour b a

(zp,zn) = partition ((<0).snd.fst) deltas


msg s d = Draw [winTitle s, Draw d]

shOrig :: (Polyline, [Int]) -> Drawing
shOrig (p, os) = Draw [Draw p, color white $ drawThings (polyPts p) (zip [0..] os)]

drawThings pts xs = draws $ zipWith (textF Helvetica10) pts (map ((' ':).show) xs)


drawDelta = Draw . map shDelta
  where
    shDelta ((_,oa),bs) = color c bs
      where
        c | oa < 0 = blue
          | otherwise = red


import Contours
import Contours.Clipping
import Numeric.LinearAlgebra
import Util.Geometry
import Data.List(minimumBy,sort)
import Data.Function(on)
import Vision.GUI.Simple
import Util.Homogeneous


a = Closed [Point 0 0, Point 1 0, Point 1 1, Point 0 1]

b = Closed [Point a1 b1, Point a2 b1, Point a2 b2, Point a1 b2]
  where
    a1 = -1
    a2 =  2
    b1 = -1
    b2 =  2

c = Closed [Point a1 b1, Point a2 b1, Point a2 b2, Point a1 b2]
  where
    a1 =  2
    a2 =  3
    b1 = -1
    b2 =  2

d = Closed [Point a1 b1, Point a2 b1, Point a2 b2, Point a1 b2]
  where
    a1 =  -1
    a2 =  3
    b1 = 0.2
    b2 =  0.8
    
main = do
    print $ map (snd.fst) (deltaContour d a)
    print $ map (snd.fst) (deltaContour b a)
    print $ map (snd.fst) (deltaContour a b)
    print $ map (snd.fst) (deltaContour c a)
    runIt win

drawDelta = Draw . map shDelta
  where
    shDelta ((_,oa),bs) = color c bs
      where
        c | oa < 0 = blue
          | otherwise = red

msg s d = Draw [winTitle s, Draw d]

testcase x y = [ msg "original contours" [color green (f x), color white (f y)]
               , msg "delta" (drawDelta (deltaContour (f x) (f y))) ]

f = (Trust (scaling 0.25) <|)


win = browser "clipping" ds (const id)
    where
      ds =  testcase d a
         ++ testcase b a
         ++ testcase a b
         ++ testcase c a


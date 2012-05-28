import Vision.GUI
import ImagProc
import Contours
import Vision
import Contours.Clipping
import Numeric.LinearAlgebra

main = runIt $ do
    win "union"        ClipUnion
    win "intersection" ClipIntersection
    win "difference"   ClipDifference
    win "XOR"          ClipXOR

win name mode = browser name xys sh
    where
      xys = [(a,c),(c,a),(a,b),(b,a),(c,d)]
      sh k (a,b) = clearColor white 
                        [ lineWd 11 [ color lightblue a, color pink b ]
                        , lineWd 2 $ map shori zs
                        ]
        where
          zs = clip mode a b
      
pre s d = transPol (desp (d,0) <> scaling s <> desp (-0.5, -0.5))

square = Closed [Point 0 0, Point 0 1, Point 1 1, Point 1 0]

a = pre 1 0 square
b = pre 0.5 0 square
c = pre 0.5 (-0.5) square
d = pre 0.5 (0.5) square

shori z | orientedArea z < 0 = color black z
        | otherwise          = color red z


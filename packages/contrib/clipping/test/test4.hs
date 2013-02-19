import Contours.Clipping
import ImagProc.Base
import Contours

p = Closed [ Point {px = 0.15270079428773498, py = 0.43295977213554554}
           , Point{px = -0.18595359038634324, py = 0.43571411250556147}
           , Point {px = -0.18911816070477735, py = 0.2711387097388278}
           , Point {px = 0.15581365744346226, py = 0.2701204873503819} ]

q = Closed [ Point {px = 0.15270079428773498, py = 0.4329597721355455}
           , Point {px = -0.18595359038634324, py = 0.43571411250556147}
           , Point {px = -0.18911816070477738, py = 0.2711387097388278}
           , Point {px = 0.15581365744346232, py = 0.2701204873503819} ]

test = sum $ map area $ clip ClipIntersection p q


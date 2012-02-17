import Vision.GUI
import ImagProc

main = runIt $ browser "points & lines" xs (const Draw)
  where
    xs = [dibu1]

dibu1 :: [Drawing]
dibu1 = [ color yellow, Draw (line2p p1 p2)
        , color red, pointSz 5, points [p1, p2] ]

p1 = Point 0.5 0.5
p2 = Point 0 (-0.2)

line2p :: Point -> Point -> HLine
-- ^ line from two points
line2p (Point x1 y1) (Point x2 y2) = HLine a b c
  where
    a = 0.1
    b = 1
    c = 0


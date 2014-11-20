import Vision.GUI.Simple
import Util.Geometry
import Util.Misc(degree)

main = animate 20000 (Size 600 600) "Koch snowflake" frame

frame n = Draw
    [ -- text (Point 0.9 0.8) (show n),
      color orange . lineWd 3 
      $ snowflake (mod n 100) (mod (div n 100) 6)  (rotPoint z a p1) (rotPoint z a p2) 
    ]
  where
    p1 = Point (-0.6) (-0.4)
    p2 = Point (0.6)  (-0.4)
    z = Point 0 0
    a  = fromIntegral n * 0.1*degree


snowflake m n p1 p2 = Draw
    [ koch m n p1 p2
    , koch m n p2 p3
    , koch m n p3 p1
    ]
  where
    p3 = rotPoint p1 (60*degree) p2 
 
koch m 0 p1 p2 = Draw $ Open [p1, p2]
koch m n p1 p2 = Draw
    [ koch m  (n-1) p1 p3
    , koch m  (n-1) p3 p4'
    , koch m  (n-1) p4' p5
    , koch m  (n-1) p5 p2
    ]
  where
    p3 = interPoint (1/3) p1 p2
    p5 = interPoint (2/3) p1 p2
    p4 = rotPoint p3 (-60*degree) p5
    p6 = interPoint 0.5 p1 p2
    p4' = interPoint f p4 p6
    f = 1-fromIntegral m' / 100
    m' = if n == 1 then m else 100




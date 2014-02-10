import Vision.GUI.Simple
import Image
import Util.Geometry

main = runIt $ browser "points & lines" xs (const id)
  where
    xs = [drawing1]

drawing1 :: Drawing
drawing1 = Draw  [  color yellow (HLine 0.1 1 0)
                 ,  pointSz 5 [Point 0.5 0.5,  Point 0 (-0.2)]
                 ]


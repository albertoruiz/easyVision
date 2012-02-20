import Vision.GUI
import ImagProc

main = runIt $ browser "points & lines" xs (const id)
  where
    xs = [drawing1]

drawing1 :: Drawing
drawing1 = Draw [ color yellow, Draw (HLine 0.1 1 0)
                , color red, pointSz 5, Draw [p1, p2] ]

p1 = Point 0.5 0.5
p2 = Point 0 (-0.2)


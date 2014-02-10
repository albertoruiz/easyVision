import Vision.GUI.Simple
import Image
import Util.Geometry

main = runIt $ do
    p <- click "click points"
    w <- browser "work with them" [] (const Draw)
    connectWith g p w

g _ pts = (0, [ map (Segment (Point 0 0)) pts] )

click name = standalone (Size 400 400) name [] updts [] sh
  where
    updts = [ (key (MouseButton LeftButton), \_ p ps -> ps++[p]) ]
    sh = color yellow . drawPointsLabeled


import Vision.GUI
import ImagProc
import Numeric.LinearAlgebra
import Vision(scaling)
import Contours(poly2roi)

sel = grayscale
fun = crossCorrGray

main = run $ getBackground
         >>> observe "template" snd
         >>> observe "cross correlation" (\(x,p) -> fun p (sel x))


getBackground = clickKeep "click to set template" f g Nothing
  where
    f _ = warp 0 (Size 320 320) (scaling 2) . sel
    g x = Draw [Draw (rgb $ fst x), color white roi ]
    roi = Closed [Point 0.5 0.5, Point 0.5 (-0.5), Point (-0.5) (-0.5), Point (-0.5) 0.5]


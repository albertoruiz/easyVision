import Vision.GUI
import ImagProc
import Numeric.LinearAlgebra
import Vision(scaling)
import Contours(poly2roi)

main = run $ getBackground
         >>> observe "template" snd
         >>> observe "cross correlation" (\(x,p) -> crossCorr p (float.grayscale $ x))


getBackground = clickKeep "click to set template" f g Nothing
  where
    f _ = warp 1 (Size 320 320) (scaling 2) . float . grayscale
    h _ c = copy $ modifyROI (const (poly2roi (size x) roi)) x
      where
        x = float . grayscale $ c
    g x = Draw [Draw (rgb $ fst x), color white roi ]
    roi = Closed [Point 0.5 0.5, Point 0.5 (-0.5), Point (-0.5) (-0.5), Point (-0.5) 0.5]


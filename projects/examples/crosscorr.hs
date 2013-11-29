import Vision.GUI
import Util.Geometry ( Polyline(polyPts) )
import Image.Processing ( Image, size, crossCorr, copy, grayf, crossCorrLoc )
import Image.ROI ( topLeft, roi2poly )

sel = grayf
fun = crossCorr

main = run $ getTemplate
         >>> observe "template" snd
         >>> arr (\(x,t) -> ((sel x,t), fun t (sel x)))
         >>> observe "cross correlation" snd
         >>> observe "best match" (\((x,t),c) -> showMatch t x c)


getTemplate = clickKeep "define region and click to set template" f g Nothing
  where
    f r = setRegion r . sel
    g = Draw . sel . fst


showMatch t img corr | v > 0.5 = Draw [Draw (copy img [(t,topLeft r)])
                                      , color green . lineWd 3 $ p
                                      , text p0 (show v)
                                      ]
                     | otherwise = Draw img
  where
    (v,r) = crossCorrLoc t img corr
    p = roi2poly (size img) r
    p0 = last (polyPts p)


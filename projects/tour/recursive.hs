import Vision.GUI
import ImagProc(grayscale, warpOn, Polyline(..), Point(..), segmentLength)
import Vision.Apps.Contours(otsuContours, smoothPolyline, polygons, asSegments)
import Numeric.LinearAlgebra((<>))
import Vision(cameraFromHomogZ0,estimateHomographyRaw,ht,scaling) 
import Util.Misc(rotateLeft,posMax)

darkContours = (id &&& (otsuContours >>> (map (smoothPolyline 4) . fst .fst)))

main = run $ arr grayscale
          >>> arr darkContours
          >>> arr (id *** take 1 . polygons 10 5 (4, 4))
          >>> observe "detected" shinfo
          >>> arrL (scanl1 f)
          >>> observe "recursive" fst

shinfo (im,ps) = Draw [ Draw im
                      , (Draw . map (drawContourLabeled blue red white 2 3)) ps ]

f (a,_) (im,c:_) = (warpOn im h a, [c])
  where
    h = estimateHomographyRaw (g c) [[1,r],[-1,r],[-1,-r],[1,-r]] <> scaling 0.95
      where
        r = 0.75
        g (Closed ps) = map (\(Point x y) -> [x,y]) (up ps)
        up = rotateLeft (k+2)
        k = posMax $ map segmentLength (asSegments c)
f _ x = x

-- to do this with "delay" (or our delay') we need DoRec and ArrowLoop ITrans


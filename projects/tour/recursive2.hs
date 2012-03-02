{-# LANGUAGE Arrows #-}

import Vision.GUI
import ImagProc(grayscale, warpOn, Polyline(..), Point(..), segmentLength)
import Vision.Apps.Contours(otsuContours, smoothPolyline, polygons, asSegments)
import Numeric.LinearAlgebra((<>))
import Vision(cameraFromHomogZ0,estimateHomographyRaw,ht,scaling) 
import Util.Misc(rotateLeft,posMax)

main = run $ proc im -> do
                let g = grayscale im
                prev <- delay' -< g   -- should be s
                let cs = map (smoothPolyline 4) . fst . fst . otsuContours $ g
                    ps = take 1 . polygons 10 5 (4, 4) $ cs
                    s = f prev g ps
                (y,_,_) <- sMonitor "recursive" dr -< (s,g,ps)
                returnA -< y

-- dr would not be in scope if defined inside the proc
dr _ (s,g,ps) = [ Draw [ Draw g, (Draw . map (drawContourLabeled blue red white 2 3)) ps ]
                , Draw s
                ]

f a im (c:_) = warpOn im h a
  where
    h = estimateHomographyRaw (g c) [[1,r],[-1,r],[-1,-r],[1,-r]] <> scaling 0.95
      where
        r = 0.75
        g (Closed ps) = map (\(Point x y) -> [x,y]) (up ps)
        up = rotateLeft (k+2)
        k = posMax $ map segmentLength (asSegments c)
f _ x _ = x

-- only the previous input frame, to get the previous output we need ArrowLoop (and DoRec)


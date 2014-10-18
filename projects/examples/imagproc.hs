{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

import Vision.GUI
import Contours
import Image.Processing
import Image.ROI
import Util.Misc(vec)
import Util.Debug(debug)
import Numeric.LinearAlgebra

autoParam "DemoParam" ""  [ ("rad","Int",intParam 2 1 20)
                          , ("val","Int",intParam 0 (-255) 255)
                          , ("scale","Int",intParam 0 (-5) 5)
                          , ("sigma","Float",realParam 1 1 10)
                          ]

main = run $ withParam (,) >>> sMonitor "result" f

f droi (DemoParam{..},x) =
           [  msg "grayscale"          [ Draw g ]
           ,  msg "gaussian fixed"     [ Draw smooth ]
           ,  msg "gaussian variable"  [ Draw gsig ]
           ,  msg "median filter"      [ Draw med ]
           ,  msg "canny edges"        [ Draw (notI edges) ]
           ,  msg "Otsu threshold"     [ Draw otsu ]
           ,  msg "raw Otsu contours"  [ proi, color blue . lineWd 2 $ draws rawconts ]
           ,  msg "adaptive contours"  [ proi, color blue . lineWd 2 $ draws latconts ]
--           ,  msg "distance transform" [ Draw disTra ]
--           ,  msg "fast marching"      [ Draw fm ]
--           ,  msg "DCT"                [ Draw dctt ]
--           ,  msg "LBP"                [ proi, Draw $ dlbp / scalar 20 - 0.5 ]
           ,  msg "Histogram"          [ proi, Draw $ histn ]
           ]
  where
    msg s x  =  Draw [ Draw img, Draw x , winTitle s ]

    img    = rgb x
    g      = setRegion droi (grayscale x)
    proi   = color gray $ roi2poly (size g) (roi g)
    smooth = gauss Mask5x5 . toFloat $ g
    gsig   = gaussS sigma . toFloat $ g
    med   = filterMedian rad g
    edges  = canny (0.1,0.3) . gradients $ smooth
    otsu   = compareC8u (otsuThreshold g) IppCmpGreater g
    rawconts = contours 1000 100 $ otsu
    latconts = map reducePolyline (fst $ localContours 10 50 g)
--    disTra = (1/60) .* distanceTransform [1,1.4,2.2] (notI edges)
--    fm     = (1/60) .* fastMarching 0 (notI edges)
--    dctt   = sqrt32f . abs32f . dct . toFloat $ g
--    dlbp   = vec (lbpN 2 g)
    hist   = fromList $ histogramN [0..256] g
    histn  = hist / scalar (maxElement hist) - 0.5


{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

import Vision.GUI
import Contours
import ImagProc
import Util.Misc(vec,debug)
import Numeric.LinearAlgebra

autoParam "DemoParam" ""  [ ("rad","Int",intParam 2 1 20)
                          , ("val","Int",intParam 0 (-255) 255)
                          , ("scale","Int",intParam 0 (-5) 5)
                          ]

main = run $ withParam (,) >>> sMonitor "result" f

f roi (DemoParam{..},x) =
           [  msg "grayscale"          [ Draw g ]
           ,  msg "gaussian filter"    [ Draw smooth ]
           ,  msg "median filter"      [ Draw med ]
           ,  msg "canny edges"        [ Draw (notI edges) ]
           ,  msg "Otsu threshold"     [ Draw otsu ]
           ,  msg "raw Otsu contours"  [ proi, color blue . lineWd 2 $ draws rawconts ]
           ,  msg "adaptive contours"  [ proi, color blue . lineWd 2 $ draws latconts ]
           ,  msg "distance transform" [ Draw disTra ]
           ,  msg "DCT"                [ Draw dctt ]
           ,  msg "LBP"                [ proi, Draw $ dlbp / scalar 20 - 0.5 ]
           ,  msg "Histogram"          [ proi, Draw $ histn ]
           ]
  where
    msg s x  =  Draw [ Draw img, Draw x , winTitle s ]

    img    = rgb x
    g      = setRegion roi (grayscale x)
    proi   = color gray $ roi2poly (size g) (theROI g)
    smooth = gauss Mask5x5 . float $ g
    med   = filterMedian rad g
    edges  = canny (0.1,0.3) . gradients $ smooth
    otsu   = compareC8u (otsuThreshold g) IppCmpGreater g
    rawconts = otsuContours g
    latconts = map reducePolyline (fst $ localContours 10 g)
    disTra = (1/60) .* distanceTransform [1,1.4,2.2] (notI edges)
    dctt   = sqrt32f . abs32f . dct . float $ g
    dlbp   = vec (lbpN 2 g)
    hist   = fromList $ histogramN [0..256] g
    histn  = hist / scalar (maxElement hist) - 0.5


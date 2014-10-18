{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

import Vision.GUI
import Contours
import Image.Processing
import Image.ROI
import Util.Misc(vec,stdpix)
import Util.Debug(debug)
import Numeric.LinearAlgebra

autoParam "DemoParam" ""
    [ ("rad","Int",intParam 2 1 20)
    , ("val","Int",intParam 0 (-255) 255)
    , ("scale","Int",intParam 0 (-5) 5)
    , ("sigma","Float",realParam 1 1 10)
    , ("minArea","Double",realParam 10 1 200)
    , ("maxArea","Double",realParam 30 1 200)
    ]

data Experiment = Experiment
    { orig :: Image RGB
    , mono :: Image I8u
    , bina :: Image I8u
    , cont :: [Polyline]
    }

main = run $   withParam work
           >>> observe "source" orig
           >>> observe "grayscale" mono
           >>> observe "binary"    bina
           >>> observe "contours"  (color red . cont)

work DemoParam{..} x = Experiment {..}
  where
    orig = rgb x
    mono = grayscale x
    bina = compareC8u (otsuThreshold mono) IppCmpLess mono
    cont = filter ((\a -> a <(maxArea*stdpix)^2 && a > (minArea*stdpix)^2) . area) $ contours 1000 10 bina
    


{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import Vision.GUI
import ImagProc
import Numeric.LinearAlgebra ((<>),toList,linspace)
import Vision(kgen)
import Util.Rotation
import Util.Misc(degree)
import Contours

autoParam "ConjRotP" "cg-"
    [ ("pan",  "Double",   realParam (0) (-40) (40))
    , ("tilt", "Double",   realParam (0) (-30) (30))
    , ("roll",  "Double",  realParam  0 (-40) (40))
    , ("focal",  "Double", listParam 1.7 [0.5, 0.7, 1, 1.4, 1.7, 2, 2.5, 3, 4, 6, 10])
    ]

conjugateRotation ConjRotP{..} =
           kgen focal
        <> rot1 (tilt*degree)
        <> rot2 (pan*degree)
        <> rot3 (roll*degree) 
        <> kgen (1/focal)

main = runIt $ drawParam "conjugate rotation" drws

drws p = [ Draw [ pointSz 3, lineWd 1
                , color darkgray, Draw l1, Draw l2
                , color green, segments segs
                , color lightgreen, points ps' 
                ]
         ]
  where
    l1 = HLine 0 1 0
    l2 = HLine 1 0 0
    ps = [Point x y | x <- ran, y <- ran]
    ran = toList $ linspace 9 (-0.8,0.8)
    Open ps' = transPol (conjugateRotation p) (Open ps)
    segs = zipWith Segment ps ps'


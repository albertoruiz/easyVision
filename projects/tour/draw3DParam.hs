{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import Vision.GUI.Simple
import Image
import Util.Geometry
import qualified Graphics.UI.GLUT as GL

autoParam "Position" ""
    [ ("x",  "Double",   realParam 3 (-10) 10)
    , ("y",  "Double",   realParam 2 (-10) 10)
    , ("z",  "Double",   realParam 3 (-10) 10)
    ]

main = runIt $ draw3DParam "3D drawing" drws

drws Position{..} =
    [ clearColor white . blend $
        [ color black . lineWd 2 $ axes3D 5
        , pointSz 3 . color red $ [p, p2]
        , color blue (gjoin p p2)
        , (lineWd 3 . color blue) (lineStrip [p, p2])
        , color orange l
        , color lightgray [ projs p, projs p2 ]
        , colorAlpha lightgreen 0.8 (drawPolygon pol)
        ]
    ]
  where
    p = Point3D x y z
    p2 = Point3D 1 1 1
    projs p = lineStrip
        [ Point3D x y z
        , Point3D x y 0
        , Point3D x 0 0
        , Point3D x y 0
        , Point3D 0 y 0 ]
      where
        Point3D x y z = p

    l = meet (HPlane 0 0 1 (-z)) (HPlane 0 1 0 (-1))


pol = map (meet p) ls
  where
    p = HPlane 1 1 1 (-4)
    ls = map (gjoin (Point3D 0 0 0)) [Point3D 1 0 0, Point3D 0 1 0, Point3D 0 0 1]


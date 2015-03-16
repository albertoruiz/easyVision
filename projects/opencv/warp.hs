{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import Vision.GUI.Simple
import Image
import Image.Devel(gray2float,float2gray)
import Numeric.LinearAlgebra ((<>),ident)
import Vision(ht,desp,scaling,kgen)
import Util.Rotation
import Util.Misc(degree)
import Util.Geometry(Polyline(..))
import qualified OpenCV

autoParam "CGParam" "cg-"
    [ ("pan",  "Double",   realParam (0) (-40) (40))
    , ("dx",  "Double",    realParam (0) (-1) (1))
    , ("dy",  "Double",    realParam (0) (-1) (1))
    , ("rot", "Double",    realParam (0) (-50) (50))
    , ("tilt", "Double",   realParam (0) (-20) (80))
    , ("roll",  "Double",  realParam  0 (-40) (40))
    , ("focal",  "Double", listParam 1.6 [0.5, 0.7, 1, 1.6, 1.8, 2, 2.4, 2.8, 3.5, 5, 7, 9 ,12])
    , ("scale",  "Double", listParam 1 [1.05**k|k<-[-40..10]])]

main = run $    arr rgb -- fCh
           >>>  deskew @@@ winParam
           >>>  observe "warped" drw

deskew par@CGParam{..} img = OpenCV.warp (Word24 0 0 48) (size img) r img
                             
  where
    h = conjugateRotation par
    [[a,b]] = ht h [[dx,-dy]]
    r = desp (-a,-b) <> h

conjugateRotation CGParam{..} =
        scaling scale
        <> rot3 (rot*degree)
        <> kgen focal
        <> rot1 (tilt*degree)
        <> rot2 (pan*degree)
        <> rot3 (roll*degree) 
        <> kgen (1/focal)

drw img = Draw [Draw img, color orange [Open [a,b,z,c,d], Open [x1,x2], Open [y1,y2] ]]
  where
    a = Point (-1) 0
    b = Point 1 0
    c = Point 0 (-1)
    d = Point 0 1
    z = Point 0 0
    x1 = Point (-0.5) (-0.1)
    x2 = Point (-0.5) (0.6)
    y1 = Point (0.1) 0.5
    y2 = Point (-0.6) 0.5


{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import Vision.GUI.Simple
import Image
import Image.Devel(gray2float,float2gray)
import Numeric.LinearAlgebra ((<>),ident)
import Vision(ht,desp,scaling,kgen)
import Util.Rotation
import Util.Misc(degree)
import qualified OpenCV

autoParam "CGParam" "cg-"
    [ ("pan",  "Double",   realParam (0) (-40) (40))
    , ("dx",  "Double",    realParam (0) (-1) (1))
    , ("dy",  "Double",    realParam (0) (-1) (1))
    , ("tilt", "Double",   realParam (15) (-30) (30))
    , ("roll",  "Double",  realParam  20 (-40) (40))
    , ("focal",  "Double", listParam 2.8 [0.5, 0.7, 1, 2, 2.6, 2.8, 5, 5.5, 9,10])
    , ("scale",  "Double", listParam 0.8 [1.05**k|k<-[-20..20]])]

main = run $    arr rgb -- fCh
           >>>  deskew @@@ winParam
           >>>  observe "warped" id

deskew par@CGParam{..} img = -- float2gray $ OpenCV.warp32f 0.5 (size img) r img
                             OpenCV.warp8u3 32 (size img) r img
  where
    h = conjugateRotation par
    [[a,b]] = ht h [[dx,-dy]]
    r = desp (-a,-b) <> h

conjugateRotation CGParam{..} =
        scaling scale
        <> kgen focal
        <> rot1 (tilt*degree)
        <> rot2 (pan*degree)
        <> rot3 (roll*degree) 
        <> kgen (1/focal)


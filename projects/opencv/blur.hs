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

autoParam "CGParam" ""
    [ ("sigma",  "Double",  realParam (1) (0) (20))
    ]

main = run $    arr yCh
           >>>  blur @@@ winParam
           >>>  observe "blur" id

blur par@CGParam{..} img = OpenCV.gaussianBlur sigma img


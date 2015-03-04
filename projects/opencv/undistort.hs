{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import Vision.GUI.Simple
import Image
import Numeric.LinearAlgebra.HMatrix
import qualified OpenCV

autoParam "RDParam" "cg-"
    [ ("k1",  "Double",   realParam (0) (-1) 1)
    ]

main = run $    arr yCh
           >>>  observe "original" id
           >>>  undistort @@@ winParam
           >>>  observe "undistorted" id

undistort par@RDParam{..} img = OpenCV.undistort8u k d k img
  where
    f = 1.6
    k = matrix 3 [f*320, 0    , 320
                 ,  0  , f*320, 240
                 ,  0  ,    0,   1 ]
    d = vector [k1,0,0,0]


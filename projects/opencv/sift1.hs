{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import Vision.GUI.Simple
import Image
import Image.ROI(roi2poly)
import qualified OpenCV
import Image.Devel(rgb2gray,constantImage)


import Numeric.LinearAlgebra
import Util.Geometry

autoParam "SiftParam" ""
    [ ("maxFeat",  "Double",   listParam 0 [0, 10, 100, 1000])
    , ("contrastTh", "Double", realParam 0.04 0 0.2)
    , ("edgeTh", "Double", realParam 5 0 20)
    ]

work (p@SiftParam {..}) x = ((rgb x, p), OpenCV.sift (round maxFeat) contrastTh edgeTh (yCh x))

main = run  $  work @@@ winParam
           >>> observe "SIFT keypoints" sh2

sh2 ((x,_),(rlocs,_)) = Draw
    [ Draw x
    , color blue . lineWd 2 $ pts
    , color red $ text (Point 0.95 0.65) (show (rows rlocs))
    ]
  where
    pts = unsafeMatDat rlocs :: [KeyPoint]


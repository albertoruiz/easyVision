{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import Vision.GUI
import Image

autoParam "Position" ""
    [ ("x",  "Double",   realParam (0) (-1) 1)
    , ("y",  "Double",   realParam (0) (-1) 1)
    ]

main = runIt $ drawParam "my drawing" drws

drws Position{..} =
    [ color red . pointSz 3 $ Point x y
    ]


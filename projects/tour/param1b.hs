{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import Vision.GUI
import ImagProc

autoParam "Param" "g-"  [  ("sigma","Float",realParam 3 0 20)
                        ,  ("scale","Float",realParam 1 0 5) ]

main = run  $    arr grayscale
            >>>  g @@@ winParam
            >>>  observe "gauss" id

g Param{..} = (scale .*) . gaussS sigma . float



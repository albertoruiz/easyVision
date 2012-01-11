{-# LANGUAGE TemplateHaskell #-}

import EasyVision.GUI
import ImagProc

autoParam "Param" "" [("sigma","Float",realParam 3 0 20)]

main = run   (winParam .&. camera) 
         $   (arr sigma *** arr (float . grayscale))
        >>>  observe "gauss" (uncurry gaussS)


{-# LANGUAGE TemplateHaskell #-}

import Vision.GUI
import ImagProc
import Util.LazyIO((.&.))

autoParam "Param" "" [("sigma","Float",realParam 3 0 20)]

main = runT_ (winParam .&. camera) 
         $    (arr sigma *** arr (float . grayscale))
         >>>  observe "gauss" (uncurry gaussS)


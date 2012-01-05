{-# LANGUAGE TemplateHaskell #-}

import EasyVision

autoParam "Param" "" [("sigma","Float",realParam 3 0 20)]

main = run $    (winParam ~> sigma .&. camera ~> float . grayscale)
           >>=  observe "gauss" (uncurry gaussS)


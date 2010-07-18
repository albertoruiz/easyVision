{-# LANGUAGE TemplateHaskell #-}

import EasyVision

$(autoParam "Param" "param" "WParam" ""
    [("sigma","Float",realParam 3 0 20)]
 )

main = run $ (param ~> sigma .&. camera ~> float . gray) >>= observe "gauss" (uncurry gaussS)

{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import EasyVision

autoParam "Param" "g-" [ ("sigma","Float",realParam 3 0 20)
                       , ("scale","Float",realParam 1 0 5) ]

main = run $    camera ~> grayscale >>= g .@. winParam
           >>=  observe "gauss" snd

g Param{..} = (scale .*) . gaussS sigma . float


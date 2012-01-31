{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import Vision.GUI
import ImagProc
import Util.Options
import Control.Applicative

autoParam "SParam" "g-"  [  ("sigma","Float",realParam 3 0 20)
                         ,  ("scale","Float",realParam 1 0 5) ]

instance ParamRecord SParam where
    defParam = defSParam
    argParam = argSParam
    winParam = winSParam  

main = run  $    arr grayscale
            >>>  withParam g
            >>>  observe "gauss" id

g SParam{..} = (scale .*) . gaussS sigma . float


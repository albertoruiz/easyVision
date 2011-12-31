{-# LANGUAGE TemplateHaskell #-}

import EasyVision

autoParam "Param" "emph-" [("radius" ,"Int" ,intParam 10 0 30)]

emph :: Int -> ImageGray -> ImageGray
emph r x = sh d
  where
    f = float x
    s = filterBox r r f
    d = f |-| s
    sh = scale32f8u (-1) 1

main = run $   camera ~> grayscale
           >>= observe "orig" id
           >>= (emph . radius) .@. winParam
           >>= monitorScanLine "emph" snd
           >>= timeMonitor


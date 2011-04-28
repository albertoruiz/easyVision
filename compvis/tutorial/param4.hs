{-# LANGUAGE TemplateHaskell #-}

import EasyVision

drift (_,a) (r,b) = (r, r .* a |+| (1-r) .* b)

autoParam "Param" "" [("alpha","Float",realParam 0.9 0 1)]

main = run $ (winParam ~> alpha .&. camera ~> float . grayscale)
           ~~> scanl1 drift
           >>= mon
           >>= timeMonitor

mon = monitor "drift with param" (mpSize 10) sh
  where
    sh (a,im) = do
        drawImage' im
        text2D 30 30 (show a)


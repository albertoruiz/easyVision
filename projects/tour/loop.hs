{-# LANGUAGE Arrows #-}

import Vision.GUI
import ImagProc

main = run  $    observe "source" rgb
            >>>  f
            >>>  observe "result"  (5.*)

f = proc img -> do
    let x = (float . grayscale) img
    p <- delay' -< x
    returnA -< x |-| p


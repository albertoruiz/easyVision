{-# LANGUAGE Arrows #-}

import Vision.GUI
import Image.Processing

main = run  $    observe "source" rgb
            >>>  f
            >>>  observe "result"  (5.*)

f = proc img -> do
    let x = (toFloat . grayscale) img
    p <- delay' -< x
    returnA -< x |-| p


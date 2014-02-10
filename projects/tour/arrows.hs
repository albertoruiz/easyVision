{-# LANGUAGE Arrows #-}

import Vision.GUI
import Image.Processing

main = run  $    observe "source" rgb
            >>>  arr grayscale
            >>>  p
            >>>  observe "result"  (5.*)

p = proc g -> do
    let f = toFloat g
    x <- observe "x" id -< f
    s <- (observe "s" id <<< arr (gaussS 5)) -< f
    observe "inverted" notI -< g
    returnA -< x |-| s


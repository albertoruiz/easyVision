{-# LANGUAGE Arrows #-}

import Vision.GUI
import ImagProc

main = run  $    arrL (zip [0..])
            >>>  separ
            >>>  observe "final" rgb

separ = proc (k,img) -> do
    if odd (k `div` 25)
        then observe "monochrome" grayscale -< img
        else observe "negated" (notI . grayscale) -< img

{-# LANGUAGE Arrows #-}

import EasyVision.GUI
import ImagProc

main = run $    observe "orig" rgb
           >>>  arr grayscale
           >>>  f
           >>>  observe "result"  (5.*)

f = proc x -> do
    let f = float x
    s0 <- observe "s0" id -< f
    s1 <- (observe "s1" id <<^ gaussS 5) -< f
    z <- observe "negated (not used)" notI -< x
    returnA -< s0 |-| s1


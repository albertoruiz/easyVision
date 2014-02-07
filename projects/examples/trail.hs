{-# LANGUAGE Arrows, RecursiveDo #-}

import Vision.GUI
import Image.Processing

main = run  $ arr grayf
            >>> optDo "-v" (observe "source" id)
            >>> mix 3 (    tempdiff 3
                       >>> optDo "-v" (observe "tempdiff" gg)
                       >>> drift 0.95
                       >>> optDo "-v" (observe "drift" (5 .*))
                      )
            >>> observe "trail" id


drift alpha = proc x -> do 
    rec py <- delay zero -< y
        let y = alpha .* py |+| (1-alpha) .* x
    returnA -< y

tempdiff scale = proc x -> do
    px <- delay zero -< x
    returnA -< (scale) .* thresholdVal 0 0 IppCmpLess (x |-| px)

zero = constantImage 0 (Size 480 640)

mix alpha p = proc x -> do
    r <- p -< x
    returnA -< x |+| alpha .* r

gg = scale32f8u (-1) 1


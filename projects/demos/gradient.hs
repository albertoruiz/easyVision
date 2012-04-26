{-# LANGUAGE Arrows, DoRec, RecordWildCards, TemplateHaskell, BangPatterns #-}

import Vision.GUI
import ImagProc
import Vision.Apps.Show(showVectorField)

autoParam "Smooth" "s-"  [  ("sigma","Float",realParam 8 0 20) ]


main = run  $ arr (float . grayscale) >>> shGrad

shGrad = withParam f >>> showVectorField "gradient" g
  where
    f Smooth {..} x = gaussS sigma x
    g x = (x,(sc.*gx g, (-sc).*gy g))
      where
        g = gradients x
        sc = 0.5


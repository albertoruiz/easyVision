{-# LANGUAGE Arrows, RecordWildCards, TemplateHaskell #-}

import Vision.GUI
import Image.Processing

autoParam "Smooth" "s-"  [  ("sigma","Float",realParam 8 0 20) ]


main = run  $ arr (toFloat . grayscale) >>> shGrad

shGrad = withParam f >>> showVectorField "gradient" g
  where
    f Smooth {..} x = gaussS sigma x
    g x = (x,(sc.*gx g, (-sc).*gy g))
      where
        g = gradients x
        sc = 0.5


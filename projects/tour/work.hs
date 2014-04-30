{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

import Vision.GUI
import Image.Processing

autoParam "DemoParam" ""
    [ ("sigma","Float",realParam 5 1 10)
    ]

data Experiment = Experiment
    { orig   :: Image RGB
    , mono   :: Image Float
    , smooth :: Image Float
    }

work DemoParam{..} x = Experiment {..}
  where
    orig = rgb x
    mono = grayf x
    smooth = gaussS sigma mono


main = run $   withParam work
           >>> observe "source"    orig
--         >>> observe "smooth"    smooth
           >>> sMonitor "grayscale" sh

sh _roi Experiment{..} = [Draw smooth, Draw mono]



{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

import Vision.GUI
import ImagProc

autoParam "SParam" "g-"  [  ("radius","Int",intParam 2 0 10) ]

main = run  $    arr grayscale
            >>>  withParam (,)
            >>>  observe "median filter" sh >>> freqMonitor

sh (SParam{..}, x) = filterMedian radius x


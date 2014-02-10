{-# LANGUAGE RecordWildCards #-}


import Vision.GUI
import Image.Processing
import Vision.GUI.Draw
import Numeric.LinearAlgebra

main = run $ sMonitor "image" sh
  where
    sh _ x = [ Draw (rgb x), Draw x ]


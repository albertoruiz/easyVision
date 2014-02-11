import Vision.GUI
import Image.Processing

main = run $ sMonitor "image" sh
  where
    sh _ x = [ Draw (rgb x), Draw x ]


import Vision.GUI
import ImagProc

main = run $ arr rgb
           >>> observe "original" id
           >>> arr (modifyROI (shrink (150,200)))
           >>> observe "source" id
           >>> observe "resize" (resize (Size 50 50))


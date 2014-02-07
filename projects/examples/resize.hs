import Vision.GUI
import Image.Processing
import Image.ROI(shrink)

main = run $ arr rgb
           >>> observe "original" id
           >>> arr (modifyROI (shrink (150,200)))
           >>> observe "source" id
           >>> observe "resize" (resize (Size 50 50))
           >>> observe "resizeFull" (resizeFull (Size 120 160))


import Vision.GUI
import Image.Processing
import Util.Debug(debug)

main = run $ observe "image" rgb
           >>> arr (debug "S" (sumPixels.grayscale))


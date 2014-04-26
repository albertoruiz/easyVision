import Vision.GUI
import Image.Processing
import Util.Misc(debug)

main = run $ observe "image" rgb
           >>> arr (debug "S" (sumPixels.grayscale))


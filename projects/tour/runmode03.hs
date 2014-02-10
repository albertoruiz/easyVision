import Vision.GUI
import Image.Processing

f = sumPixels . grayscale

main = runT camera (observe "Image" rgb >>> arr f) >>= print

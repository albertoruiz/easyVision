import Vision.GUI
import ImagProc

f = sum8u . grayscale

main = runT camera (observe "Image" rgb >>> arr f) >>= print

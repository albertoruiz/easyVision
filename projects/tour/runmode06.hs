import Vision.GUI
import ImagProc

f = sum8u . grayscale

main = runS camera (arr f) >>= print . sum

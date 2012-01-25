import Vision.GUI
import ImagProc

main = run p

p = observe "RGB" rgb >>> arr grayscale >>> observe "inverted" notI


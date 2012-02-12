import Vision.GUI
import ImagProc

main = run $ observe "img" rgb >>> arrIO (print . size . grayscale)


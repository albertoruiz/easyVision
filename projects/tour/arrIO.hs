import Vision.GUI
import Image.Processing

main = run $ observe "img" rgb >>> arrIO (print . size . grayscale)


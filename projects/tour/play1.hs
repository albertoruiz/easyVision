import Vision.GUI
import Image.Processing

main = run p

p = observe "RGB" rgb >>> arr grayscale >>> observe "inverted" notI


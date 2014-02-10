import Vision.GUI
import Image.Processing

f = sumPixels . grayscale

main = runS camera (arr f) >>= print . sum


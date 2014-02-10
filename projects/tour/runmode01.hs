import Vision.GUI
import Image.Processing

f = sumPixels . grayscale

main = do
    rs <- runS camera $ arr f
    print rs


import Vision.GUI
import Image.Processing

f = sumPixels . grayscale

main = do
    prepare
    rs <- runNT camera $ observe "Image" rgb >>> arr f
    print rs

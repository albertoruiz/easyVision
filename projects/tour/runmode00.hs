import Vision.GUI
import Image.Processing

f = sumPixels . grayscale

main = do
    cam <- camera
    mbimg <- cam
    print (f `fmap` mbimg)


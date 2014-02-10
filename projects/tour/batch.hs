import Vision.GUI
import Image.Processing

main = do
    x <- runT camera  $    observe "image" rgb
                      >>>  arr (sumPixels.grayscale)
    print (sum $ take 1000 x)


import Vision.GUI
import ImagProc

main = do
    x <- runT camera  $    observe "image" rgb
                      >>>  arr (sum8u.grayscale)
    print (sum $ take 1000 x)


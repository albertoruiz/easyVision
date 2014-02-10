import Vision.GUI
import Image.Processing
import Util.Options

f = sumPixels . grayscale

main = do
    n <- getOption "--skip" 0
    prepare
    r <- runNT camera $ arrL (drop n) >>> observe "image" rgb >>> arr f
    print (length r)


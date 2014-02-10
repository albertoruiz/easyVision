import Vision.GUI
import Image.Processing

main = do
    r <- runS camera $ arr (size . grayscale)
    print $ take 10 r


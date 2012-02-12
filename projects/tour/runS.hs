import Vision.GUI
import ImagProc

main = do
    r <- runS camera $ arr (size . grayscale)
    print $ take 10 r


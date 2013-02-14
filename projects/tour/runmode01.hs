import Vision.GUI
import ImagProc

f = sum8u . grayscale

main = do
    rs <- runS camera $ arr f
    print rs

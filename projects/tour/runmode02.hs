import Vision.GUI
import ImagProc

f = sum8u . grayscale

main = do
    prepare
    rs <- runNT camera $ observe "Image" rgb >>> arr f
    print rs

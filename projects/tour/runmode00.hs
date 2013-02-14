import Vision.GUI
import ImagProc

f = sum8u . grayscale

main = do
    cam <- camera
    mbimg <- cam
    print (f `fmap` mbimg)

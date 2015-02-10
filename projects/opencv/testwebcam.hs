import Vision.GUI.Simple
import qualified OpenCV

cam = OpenCV.webcam "/dev/video1" (Size 600 800) 30

main = do
    runT_ cam (observe "source" id  >>> freqMonitor)

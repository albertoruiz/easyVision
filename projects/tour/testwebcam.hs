import Vision.GUI.Simple
import Image.Capture
import Image.Devel

cam = webcam "/dev/video1" (Size 600 800) 30

main = do
    runT_ cam (observe "source" yuyv2rgb  >>> freqMonitor)

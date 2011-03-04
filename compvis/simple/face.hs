import OpenCV
import EasyVision

main = run (camera ~> gray >>= face >>= observe "Face Detector" id >>= timeMonitor)

face cam = do
    detect <- cascadeClassifier "../../data/haarcascades/haarcascade_frontalface_alt.xml"
    return $ do
         x <- cam
         let r = detect x 1
         if null r then return x
                   else return $ modifyROI (const $ head r) x


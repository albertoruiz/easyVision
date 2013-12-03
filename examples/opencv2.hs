import Vision.GUI.Simple
import Image
import Image.Capture(webcam)
import Image.Devel(yuyv2gray)
import Image.ROI(roi2poly)
import qualified OpenCV

main = do
    faceDetector <- OpenCV.cascadeClassifier "/usr/share/opencv/haarcascades/haarcascade_frontalface_alt.xml"
    runT_ (webcam "/dev/video1" (Size 480 640) 30)
        $   arr yuyv2gray
        >>> observe "opencv face detector" (sh faceDetector)

sh f x = Draw [ Draw x, lineWd 3 faces ]
  where
    faces = map (roi2poly (size x)) (f x 1)


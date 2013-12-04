import Vision.GUI.Simple
import Image
import Image.Devel(rgb2gray)
import Image.ROI (roi2poly)
import qualified OpenCV

main = do
    faceDetector <- OpenCV.cascadeClassifier "/usr/share/opencv/haarcascades/haarcascade_frontalface_alt.xml"
    run $ observe "opencv face detector" (sh faceDetector)

sh f x = Draw [ Draw g, lineWd 3 faces ]
  where
    faces = map (roi2poly (size g)) (f g 1)
    g = rgb2gray x


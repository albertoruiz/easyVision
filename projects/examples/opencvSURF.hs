import Vision.GUI.Simple
import Image
import Image.ROI(roi2poly)
import qualified OpenCV

main = do
    run $ arr yCh
        >>> observe "opencv SURF detector" (shSURF)

shSURF g = Draw [Draw g, color red . pointSz 5 $ pts]
  where
    pts = OpenCV.surf 1000 g


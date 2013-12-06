import Vision.GUI
import Image.Capture
import Image.Processing
import Contours
import Util.Geometry
import Util.Homogeneous

main = do
    [img] <- readImages ["../../data/images/text.png"]
    let x = rgbToGray img
        cs = contours 1000 50 (notI x)
    runIt $  browser "text" [x] (const Draw)
          >> browser "glyphs" (map (redu . normalShape) cs) (const Draw)
  where
    redu = ((unsafeFromMatrix (scaling 0.4) :: Homography) <|)


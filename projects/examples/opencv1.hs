import Vision.GUI.Simple
import Image.Capture ( readImages )
import Image.Devel ( rgb2gray )
import qualified OpenCV ( hough, canny )

main = do
    [img] <- readImages ["../data/images/calibration/cube3.png"]
    let x = rgb2gray img
        y = OpenCV.canny x
        seg = OpenCV.hough 50 y

    runIt $ browser "canny" [y,x] (const Draw)
         >> browser "hough" [Draw [Draw x, color red $ seg]] (const id)


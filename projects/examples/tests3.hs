import Vision.GUI.Simple
import Image.Capture ( readImages )
import Image.Processing ( rgbToGray )
import qualified OpenCV ( hough, canny )


main = do
    [img] <- readImages ["../data/images/calibration/cube3.png"]
    let x = rgbToGray img
        y = OpenCV.canny x
        seg = OpenCV.hough 50 y
        --z = modifyROI (roiArray 2 1 1 1) x

    runIt $ browser "canny" [y,x] (const Draw)
         >> browser "hough" [Draw [Draw x, color red $ seg]] (const id)


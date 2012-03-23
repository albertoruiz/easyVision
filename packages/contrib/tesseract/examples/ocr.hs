import Vision.GUI
import ImagProc
import ImagProc.Contrib.Tesseract
import Contours.Base(setRegion)

main = run $ arr grayscale >>> clickOCR

clickOCR = clickKeep "OCR" f sh (Just "")
  where
    f roi x = tesseract (setRegion roi x)
    sh (x,s) = Draw [ Draw x, text (Point 0.9 0) s ]


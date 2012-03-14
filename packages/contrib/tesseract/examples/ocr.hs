import Vision.GUI
import ImagProc
import ImagProc.Contrib.Tesseract
import Contours.Base(setRegion)

main = run $ arr grayscale >>> clickOCR

clickOCR :: ITrans ImageGray ImageGray
clickOCR = transUI $ interface (Size 240 320) "OCR" ("",False) ft updt [] r sh
  where
    r _ (s,False) input = ((s,False), input)
    r roi (s,True) input = ((ocr roi input, False), input)
    sh _ (s,_) x = Draw [ Draw x, text (Point 0.9 0) s ]
    updt = [(key (MouseButton LeftButton), \roi _ (s,_) -> (s,True) )]
    ft _ _ = return ()

ocr roi x = tesseract (setRegion roi  x)


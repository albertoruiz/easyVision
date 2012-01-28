import Vision.GUI
import Contours
import ImagProc

main = run (sMonitor "result" f >>> freqMonitor)

f roi x =  [  msg "grayscale"         [ Draw g ]
           ,  msg "gaussian filter"   [ Draw smooth ]
           ,  msg "median filter"     [ Draw med ]
           ,  msg "canny edges"       [ Draw (notI edges) ]
           ,  msg "Otsu threshold"    [ Draw otsu ]
           ,  msg "black contours"    [ color blue, lineWd 2, draws conts ]
           ]
  where
    msg s x  =  Draw [ Draw img, Draw x , winTitle s ]
    draws  = Draw . map Draw

    img    = rgb x 
    g      = setRegion roi (grayscale x)
    smooth = gauss Mask5x5 . float $ g
    med    = median Mask5x5 g
    edges  = canny (0.1,0.3) . gradients $ smooth
    otsu   = compareC8u (otsuThreshold g) IppCmpGreater g
    ((conts,_),_)  = localOtsuContours 1 g


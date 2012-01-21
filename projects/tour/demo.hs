import EasyVision.GUI
import Contours.Base
import ImagProc

main = run (sMonitor "result" f >>> freqMonitor)

f roi x =  [  msg "grayscale"         [  Draw g ]
           ,  msg "gaussian filter "  [  Draw smooth ]
           ,  msg "canny edges"       [  Draw (notI edges) ] ]
  where
    img  =  rgb x 
    g    =  setRegion roi (grayscale x)
    smooth  =  gauss Mask5x5 . float $ g
    edges  =  canny (0.1,0.3) . gradients $ smooth

    msg s x  =  Draw [ Draw img, Draw x , color yellow, text (Point 0.9 0.65) s ]


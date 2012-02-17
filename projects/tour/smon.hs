import Vision.GUI
import Contours.Base
import ImagProc

main = run $ sMonitor "result" f 

f roi x =  [  msg "grayscale"         [  Draw g ]
           ,  msg "gaussian filter "  [  Draw smooth ]
           ,  msg "canny edges"       [  Draw (notI edges) ] ]
  where
    img  =  rgb x 
    g    =  setRegion roi (grayscale x)
    smooth  =  gauss Mask5x5 . float $ g
    edges  =  canny (0.1,0.3) . gradients $ smooth

    msg s t  =  Draw [ Draw img, Draw t , color yellow, text (Point 0.9 0.65) s ]


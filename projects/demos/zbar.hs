import Vision.GUI
import ImagProc
import ImagProc.Contrib.ZBar

main = run $ observe "zbar" (f.grayscale)

f x = Draw  [  Draw x
            ,  color blue, text (Point 0 0) (concatMap show (zbar x)) ]


import Vision.GUI
import ImagProc
import ImagProc.Contrib.Examples

main = run $ observe "C wrapper test" (f.grayscale)

f x = Draw [ Draw (invertInC x)
           , text (Point 0 0) (show (sum8u x, sumInC x)) ]


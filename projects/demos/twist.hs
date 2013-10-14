import Vision.GUI
import ImagProc
import Numeric.LinearAlgebra

main = run $ observe "source" rgb
           >>> observe "twisted" (twistColors (toLists m) . rgb)

m = (3><4)
    [ 0.5, 0.1, 0.1, 0.1
    , 0.1, 0.5, 0.1, 0.1
    , 0.1, 0.1, 0.5, 0.1 ] :: Matrix Float


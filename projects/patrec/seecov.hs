import Vision.GUI.Simple
import Util.Geometry
import Numeric.LinearAlgebra.HMatrix
import Util.Gaussian
import Util.Debug(debug)


main :: IO ()
main = do
    runIt $ clickPoints "click points" "--points" () sh

sh :: ([Point], ()) -> Drawing
sh (ps,()) = clearColor white
    [ whenD okc drwc
    , whenD okm drwm
    , pointSz 3 . color red  $ ps
    ]
  where
    (m,c) = meanCov $ datMat $ ps
    mp = unsafeFromVector m :: Point
    drwm = pointSz 5 . color blue $ mp
    drwc = color blue $ map (flip ellipCov2D (N m c)) [0.75,1.5,3]
    okm = length ps > 0
    okc = length ps > 2

whenD b d = if b then d else Draw ()


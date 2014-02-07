import Vision.GUI.Simple
import Util.Geometry
import Numeric.LinearAlgebra
import Util.Gaussian

main :: IO ()
main = do
    runIt $ clickPoints "click points" "--points" () sh

sh :: ([Point], ()) -> Drawing
sh (ps,()) = Draw [ if okc then drwmix else Draw ()
                  , pointSz 3 . color red  $ ps
                  ]
  where
    x = datMat ps
    (m,c) = meanCov x
    mp = unsafeFromVector m :: Point
    drwm = pointSz 5 . color blue $ mp
    drwc = color blue $ ellipCov2D 2 (N m c)
    okm = length ps > 0
    okc = length ps > 3
    mix = findMixture x
    drwmix = color blue $ map (ellipCov2D 2 . snd) mix


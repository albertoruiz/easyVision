import Vision.GUI.Simple
import Util.Geometry
import Numeric.LinearAlgebra
import Util.Gaussian ( Gaussian(N) )
import Util.Misc ( degree )
import Util.Rotation ( rot3 )
import Util.Homogeneous (desp)


main :: IO ()
main = do
    runIt $ clickPoints "click points" "--points" () sh


sh :: ([Point], ()) -> Drawing
sh (ps,()) = Draw [ if okc then drwc else Draw ()
                  , if okm then drwm else Draw ()
                  , pointSz 3 . color red  $ ps
                  ]
  where
    (m,c) = meanCov $ datMat ps
    mp = unsafeFromVector m :: Point
    drwm = pointSz 5 . color blue $ mp
    drwc = color blue $ ellipCov (N m c)
    okm = length ps > 0
    okc = length ps > 2


ellipCov :: Gaussian -> Polyline
ellipCov (N m c) = h ◁ circle
  where
    angle = atan2 vy vx
    (l,v) = eigSH' c
    [d1,d2] = toList (sqrt l)
    [vx,vy] = toList $ head (toColumns v)
    circle = Closed [Point (2*d1*cos(t*degree)) (2*d2*sin(t*degree)) | t <- [ 0, 10 .. 350 ]]
    h = unsafeFromMatrix $ desp (m@>0,m@>1) <> rot3 (-angle) :: Homography


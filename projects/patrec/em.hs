import Numeric.LinearAlgebra
import Util.Gaussian
import Util.Misc(vec,mat,Mat,debug,degree)
import Util.Covariance
import Util.Rotation(rot3)
import Util.Homogeneous(ht)
import Util.Optimize(optimize)

import Vision.GUI
import Util.ScatterPlot
import ImagProc
import Contours.Base

import Data.Maybe(maybe)
import System.Random(randomIO)
import Text.Printf(printf)
import Control.Monad((>=>))
import Control.Arrow((&&&),(***))
import Data.List(sortBy)
import Data.Function(on)

----------------------------------------------------------------------

disp = putStr . dispf 2

scw title p mix = browser title xs (const id)
  where
    xs = [scatter p (0,1) [] (shMix mix) ]

scws title p mixs = browser title xs (const id)
  where
    xs = map (\m -> scatter p (0,1) [] (shMix m) ) mixs

shMix mix = lineWd 3 . color black . map (drawGaussian.snd) $ mix

drawGaussian g = Draw (ellipCov g)
  where
    ellipCov (N m c) = transPol ((3><3)[1,0,(m@>0),0,1,(m@>1),0,0,1] <> rot3 (-angle)) circle
      where
        angle = atan2 vy vx
        (l,v) = eigSH' c
        [d1,d2] = toList (sqrt l)
        [vx,vy] = toList $ head (toColumns v)
        circle = Closed [Point (2*d1*cos(t*degree)) (2*d2*sin(t*degree)) | t <- [ 0, 10 .. 350 ]]

----------------------------------------------------------------------

cl1 = N (vec [0,0]) ((2><2) [2,0,0,1])
cl2 = N (vec [5,5]) ((2><2) [4,-2,-2,4])
cl3 = N (vec [0,5]) ((2><2) [1,0,0,1])
cl4 = N (vec [5,0]) ((2><2) [0.5,0.2,0.2,3])

m = [(0.25,cl1), (0.25,cl2), (0.25,cl3), (0.25,cl4)]

dt = sampleMixture [7000,8000 ..] 1000 m

main = runIt $ testEM >> testEMSeq

testEMSeq = do
    let ms = emSeq dt
    mapM_ (print.snd) (take 10 ms)
    scws "EM" (map (id&&&const"1") (toRows dt)) (map fst $ take 10 ms)

testEM = do
    let f m = scw ("EM MDL "++show (length m)) (map (id&&&const"1") (toRows dt)) m
    f (findMixture dt)


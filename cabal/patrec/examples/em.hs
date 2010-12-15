import Numeric.LinearAlgebra
import Util.Gaussian
import Util.Misc(vec,mat,Mat,debug,degree)
import Util.Covariance
import Util.Rotation(rot3)
import Util.Homogeneous(ht)
import Util.Optimize(optimize)

import EasyVision hiding (debug, whitener)
import Data.Colour.Names as Col
import Graphics.UI.GLUT hiding (Size,scale)
import Data.Maybe(maybe)
import System.Random(randomIO)
import Text.Printf(printf)
import Control.Monad((>=>))
import Control.Arrow((&&&),(***))
import Data.List(sortBy)
import Data.Function(on)

----------------------------------------------------------------------

disp = putStr . dispf 2

colors = [red,blue,orange,green]++repeat Col.lightgray

runIt f = prepare >> f >> mainLoop

scw title p mix = scatterPlot title (Size 400 400) p (0,1) colors (shMix mix)

shMix mix = (lineWidth $= 3 >> setColor' black >> mapM_ drawGaussian (map snd mix))

drawGaussian g = renderPrimitive LineLoop (mapM_ vertex (ellipCov g))
  where
    ellipCov (N m c) = ht ((3><3)[1,0,(m@>0),0,1,(m@>1),0,0,1] <> rot3 (-angle)) circle
      where
        angle = atan2 vy vx
        (l,v) = eigSH' c
        [d1,d2] = toList (sqrt l)
        [vx,vy] = toList $ head (toColumns v)
        circle = [[2*d1*cos(t*degree), 2*d2*sin(t*degree)] | t <- [ 0, 10 .. 350 ]]

----------------------------------------------------------------------

cl1 = N (vec [0,0]) ((2><2) [2,0,0,1])
cl2 = N (vec [5,5]) ((2><2) [4,-2,-2,4])
cl3 = N (vec [0,5]) ((2><2) [1,0,0,1])
cl4 = N (vec [5,0]) ((2><2) [0.5,0.2,0.2,3])

m = [(0.25,cl1), (0.25,cl2), (0.25,cl3), (0.25,cl4)]

dt = sampleMixture [7000,8000 ..] 1000 m

main = testEM

testEMSeq = do
    let f m = scw ("EM "++show (length m)) (map (id&&&const"1") (toRows dt)) m
        ms = emSeq dt
    mapM_ (print.snd) (take 10 ms)
    runIt $ mapM_ (f.fst) (take 10 ms)

testEM = do
    let f m = scw ("EM MDL "++show (length m)) (map (id&&&const"1") (toRows dt)) m
    runIt $ f (findMixture dt)



import Numeric.LinearAlgebra
import Util.Gaussian
import Util.Misc ( vec )
import Vision.GUI.Simple

----------------------------------------------------------------------

disp = putStr . dispf 2

scw title p mix = browser title xs (const id)
  where
    xs = [scatter p (0,1) [] (shMix mix) ]

scws title p mixs = browser title xs (const id)
  where
    xs = map (\m -> scatter p (0,1) [] (shMix m) ) mixs

shMix mix = lineWd 3 . color black . map (drawGaussian.snd) $ mix

drawGaussian g = Draw (ellipCov2D 2 g)

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


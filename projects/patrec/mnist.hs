import Classifier
import Classifier.ToyProblems
import Util.Stat
import Classifier.Regression(msError)
import Util.ICA
import Util.Misc(debug,randomPermutation,mean)
import Util.Gaussian(mixturePDF,findMixture)

import Numeric.LinearAlgebra
import Vision.GUI
import Data.Maybe(maybe)
import System.Random(randomIO)
import Text.Printf(printf)
import Control.Monad((>=>))
import Image(Size(..),mat2img)
import Image.Processing(resize)

---------------------------------------------------------------------------

rawmnist = loadExamples "../../data/ml/mnist.txt"

main = do
    mnist <- rawmnist
    let xs = fst $ group mnist
    let gs = map (meanCov.fromRows) xs
        re = map (coordsIn (gs!!5)) (xs!!5)
    disp $ fromRows re
    print $ mean $ map (pnorm PNorm2) re

    runIt $  browser "means" (map fst gs) (const shDig)
          >> browser "mode" (f $ gs!!0) (const shDig)
          
disp = putStrLn . dispf 2          
          
rec :: (Vector Double, Matrix Double) -> ([Double] -> Vector Double)
rec (m,c) = \xs -> sum $ m : zipWith3 g xs (toList $ sqrt ls) (toColumns vs)
  where
    g x l v = scalar (x*l) * v
    (ls,vs) = eigSH c

f g = map (rec g . \x->[x] ) (toList $ linspace 20 (-2,2::Double))

coordsIn (m,c) = \v -> (v-m) <> vp
  where
    (ls,vs) = eigSH c
    vp = takeColumns 4 vs <> diag (recip $ sqrt $ subVector 0 4 ls)

---------------------------------------------------------------------------


shDigRaw v = Draw . resize (Size 200 200) . mat2img . single $ (reshape 28 v)

shDig = shDigRaw . autoscale

showImage name img = browser name [Draw img] (const id)

autoscale v = (v - scalar b) / scalar (a-b)
  where a = maxElement v
        b = minElement v

testBrowser n c = runIt $ do
    mnist <- rawmnist
    let zeros = selectClasses c mnist
    browseLabeled (concat c) zeros shDig


expand x m bs = x : r : map (scale 1) cs'  where
    ys = map (<.>(x - m)) bs
    cs = zipWith scale ys bs
    r = sum cs'
    n = fromIntegral $ length bs
    m' = scale (recip n) m
    cs' = map (+m')  cs

expand' x m bs = x : r : m : cs where
    ys = map (<.>(x - m)) bs
    cs = zipWith scale ys bs
    r = sum (m : cs)



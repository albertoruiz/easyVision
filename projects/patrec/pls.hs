import Classifier.Regression
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.HMatrix(Seed)
import Text.Printf
import Util.Statistics(mean,randomPermutation)
import Control.Monad(forM_)

disp = putStrLn . dispf 3

lat = [[l1,l2,l3] | l1<- dom, l2 <- dom, l3 <- dom]
    where dom = [-5..5]

gx [a,b,c] = [a+b,a-b+c,3*a-2*b-c,5*a+b+2*c,a-3*b-3*c] -- depends on 3 vars
gy [a,b,c] = [2*a-4*b,-a+2*b,3*a+10*b]                 -- depends on 2 vars!!

x = addNoise 100 1 $ fromLists $ map gx lat
y = addNoise 200 1 $ fromLists $ map gy lat

noise seed (r,c) = reshape c $ randomVector seed Gaussian (r*c)

addNoise :: Seed -> Double -> Matrix Double -> Matrix Double
addNoise seed sigma m = m + scalar sigma * noise seed (rows m, cols m)

prob = randomPermutation 17 $ zip (toRows x) (toRows y)

----------------------------------------------

-- leave n out
lno n cost method prob = mean cases where
    work (test,prob) = cost (method prob) test
    cases = map (work . splitAt n . flip rotate prob ) [0, n .. (length prob - n)]
    rotate n xs = drop n xs ++ take n xs

----------------------------------------------

line msg = putStrLn $ replicate 5 '-' ++ msg ++ replicate 30 '-'

study met = forM_ [1..5] $ \n -> (printf "%d: %.3f\n" n $ lno 10 msError (withNormalized (met n)) prob :: IO ())

main = do
    line ""
    printf "%.3f\n" $ msError (withNormalized mlr prob) prob
    line ""
    putStrLn . concatMap (printf "%.1f%%  ") . toList $ 100 * latent prob
    line "mlr"
    study mlrReduced
    line "pls"
    study pls
    line "pcr"
    study pcr


import Numeric.LinearAlgebra
import Util.Stat
--import Debug.Trace
import Text.Printf
import Numeric.LinearAlgebra.LAPACK
import System.Random
import Data.List(sortBy)
import Data.Function(on)

--debug x = trace (show x) x

disp m = putStrLn . format " " (printf "%.3f") $ m

vector x = Numeric.LinearAlgebra.fromList x :: Vector Double

lat = [[l1,l2,l3] | l1<- dom, l2 <- dom, l3 <- dom]
    where dom = [-5..5]

gx [a,b,c] = [a+b,a-b+c,3*a-2*b-c,5*a+b+2*c,a-3*b-3*c]
gy [a,b,c] = [2*a-4*b,-a+2*b,3*a+10*b]

x = addNoise 100 3 $ fromLists $ map gx lat
y = addNoise 200 3 $ fromLists $ map gy lat

addNoise :: Int -> Double -> Matrix Double -> Matrix Double
addNoise seed sz m = m + res where
    r = rows m
    c = cols m
    res = reshape c . fromList . take (r*c) $ randomRs (-sz, sz::Double) (mkStdGen seed)

scramble seed l = map fst $ sortBy (compare `on` snd) randomTuples where
    randomTuples = zip l (randomRs (0, 1::Double) (mkStdGen seed))

prob = scramble 17 $ zip (map toList $ toRows x) (map toList $ toRows y)

unitary v = recip (norm v) `scale` v

norm :: Vector Double -> Double
norm = pnorm PNorm2

normalize = normalizedData . stat

mean l = sum l / fromIntegral (length l)

---------------------------------------------

type Learner a b = [(a,b)] -> (a -> b)

type Vec = Vector Double
type Mat = Matrix Double

normalizer :: [Vec] -> (Mat, Vec -> Vec, Vec -> Vec)
normalizer vs = (x,f,g) where
    s = stat (fromRows vs)
    m = meanVector s
    d = sqrt (varianceVector s)
    f x = (x-m)/d
    g x = m + x*d
    x = normalizedData s

withNormalized method prob = gy . method nx ny . fx
    where
    (xs,ys) = unzip prob
    (nx,fx,_) = normalizer (map vector xs)
    (ny,_,gy) = normalizer (map vector ys)


mlr xc yc = (<>m)
    where m = linearSolveSVD xc yc

---------------------------------------------

mlrReduced n xc yc = (<>q) . (<>w)
    where m = linearSolveSVD xc yc
          (u,_,_) = svd m
          w = takeColumns n u
          q = linearSolveSVD ((xc <> w)) yc

----------------------------------------------

pls n xc yc = (<>q) . (<>w)
    where
    w = takeColumns n v
    (_,_,v) = svd (trans yc <> xc)
    t = xc <> w
    q = linearSolveSVD t yc


----------------------------------------------

pcr n xc yc = (<>q) . (<>w) where
    w = takeColumns n v
    (_,v) = eigSH (trans xc <> xc)
    t = xc <> w
    q = linearSolveSVD t yc

----------------------------------------------

mse f prob = mean [ norm (vector y - f (vector x) ) ^ 2 | (x,y) <- prob ]

mseN f prob = k * sqrt (mse f prob)
    where k = recip $ fromIntegral $ length (snd (head prob))

rotate n xs = drop n xs ++ take n xs

-- leave n out
lno n cost method prob = mean cases where
    work (test,prob) = cost (method prob) test
    cases = map (work . splitAt n . flip rotate prob ) [0, n .. (length prob - n)]

----------------------------------------------

line msg = putStrLn $ replicate 5 '-' ++ msg ++ replicate 30 '-'

for = flip mapM_

main = do
    print $ mseN (withNormalized mlr prob) prob
    line "mlr"
    for [1..5] $ \n -> printf "%d: %.3f\n" n $ lno 10 mseN (withNormalized (mlrReduced n)) prob
    line "pls"
    for [1..5] $ \n -> printf "%d: %.3f\n" n $ lno 10 mseN (withNormalized (pls n)) prob
    line "pcr"
    for [1..5] $ \n -> printf "%d: %.3f\n" n $ lno 10 mseN (withNormalized (pcr n)) prob

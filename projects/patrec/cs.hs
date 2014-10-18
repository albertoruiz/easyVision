-- simple example of L1 minimization and compressed sensing

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.HMatrix(norm_2)
import Graphics.Plot(mplot)
import Util.Misc(Mat,Vec,vec)
import Util.Statistics(randomPermutation)
import Numeric.LinearProgramming.L1
import Text.Printf(printf)
import Control.Monad(when)
import Numeric.GSL.Fourier

norm = norm_2

gaussianMatrix seed (r,c) = reshape c $ randomVector seed Gaussian (r*c) / fromIntegral c

a = (3><2) [6,0,
            0,2,
            0,1] :: Mat
b = 3 |> [3,-8,-5] :: Vec

a' = (4><3) [1,2,3,5,6,7,7,8,9,10,11,12] :: Mat
b' = vec [1..4] :: Vec

s1 = l1SolveO a b
s2 = a <\> b

c = fromLists[[2,3]]
d = fromList [4]

x = vec $ [1,-2,3] ++ replicate (1000-3) 0

m = gaussianMatrix 777 (30,1000)

y = m <> x

dispSp v = mapVectorWithIndexM_ (\k v -> when (abs v > 1E-10) (printf "%d: %.5f\n" k v :: IO ())) v

mapVectorWithIndex g = head . mapVectorWithIndexM (\a b -> [g a b])

main = do
    print s1
    print s2
    print $ a <> s1
    print $ a <> s2
    print $ pnorm PNorm1 $ a<>s1 - b
    print $ pnorm PNorm2 $ a<>s1 - b
    print $ pnorm PNorm1 $ a<>s2 - b
    print $ pnorm PNorm2 $ a<>s2 - b
    putStrLn "---------------------"
    print $ l1SolveU c d
    putStrLn "---------------------"
    dispSp $ l1SolveU m y
    putStrLn "---------------------"
    showSol

----------------------------------------------------------------------

dctBasis n = trans $ normRows $ fromColumns $ map (fst . fromComplex . subVector 0 n . (*corr) . ifft . dup) $ toColumns $ ident n
  where
    dup v = vjoin [v, rev v]
    rev = fromList . reverse . toList
    corr = fromList $ map g [0..2*fromIntegral n-1]
    g k = cis (2*pi*k*(1/4/fromIntegral n))
    normRows m = ns * m
      where
        ns = asColumn $ fromList $ map (recip . norm) (toRows m)
    
----------------------------------------------------------------------

ndim = 1000

bb = dctBasis ndim

sol = ndim |> ([0,1,0,0,1]++replicate 20 0 ++ [1]++repeat 0)

showSol = do
  dispSp estim
  mplot [fromList [1..fromIntegral ndim], bb <> sol, bb <> estim]
  print $ norm (ys - ys') / norm ys
  mplot [bb <> estim2]
  mplot [fromList [1..fromIntegral ndim], bb <> sol, bb <> estDir]
  print $ norm (zs - zs') / norm zs


measure = gaussianMatrix 778 (30,ndim) <> bb

ys = measure <> sol

ys' = (ys  + 0.00005 * randomVector 12345 Gaussian (dim ys))

estim = l1SolveU measure ys'

estim2 = measure <\> ys'

direct = fromRows $ take 30 $ randomPermutation 888 $ toRows bb

zs = direct <> sol

estDir = l1SolveU direct zs'

zs' = (zs  + 0.001 * randomVector 12345 Gaussian (dim zs))


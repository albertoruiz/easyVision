-- simple example of L1 minimization and compressed sensing

import Numeric.LinearAlgebra
import Util.Misc(Mat,Vec,vec,debug)
import Numeric.LinearProgramming
import Text.Printf(printf)
import Control.Monad(when)

-- overconstrained
l1SolveO :: Mat -> Vec -> Vec
l1SolveO a y = debug "L1" (const j) $ vec (take n x)
  where
    n = cols a
    m = rows a
    eye = ident m
    c = fromBlocks [[ a,-eye],
                    [-a,-eye]]
    d = join [y,-y]
    p = Dense $ zipWith (:<=:) (toLists c) (toList d)
    Optimal (j,x) = simplex (Minimize (replicate n 0 ++ replicate m 1)) p (map Free [1..(n+m)])


-- underconstrained
l1SolveU :: Mat -> Vec -> Vec
l1SolveU a y = debug "L1" (const j) $ vec (take n x)
  where
    n = cols a
    c1 = map (\k ->  [ 1#k, -1#k+n] :<=: 0) [1..n]
    c2 = map (\k ->  [-1#k, -1#k+n] :<=: 0) [1..n]
    c3 = zipWith (:==:) (map sp $ toRows a) (toList y)
    sp v = zipWith (#) (toList v) [1..]
    p = Sparse (c1 ++ c2 ++ c3)
    Optimal (j,x) = simplex (Minimize (replicate n 0 ++ replicate n 1)) p (map Free [1..(2*n)])


gaussianMatrix seed (r,c) = reshape c $ randomVector seed Gaussian (r*c)

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

--mapVectorWithIndex g = head . mapVectorWithIndexM (\a b -> [g a b])

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


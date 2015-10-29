module Util.Statistics(
    mean, median, quartiles, shDist,
    Histogram, histogram, meanAndSigma, credible,
    randomPermutation, randomSamples,
    MCMC(..), metropolis, metropolis', infoMetro
) where


import Numeric.LinearAlgebra.HMatrix hiding (step,idxs)
import Data.List(sortBy, sort)
import System.Random
import qualified Data.Vector as V
import Data.Function(on)
import Text.Printf
import Data.List.Split


-- | pseudorandom permutation of a list
randomPermutation :: Seed -> [a] -> [a]
randomPermutation seed l = map fst $ sortBy (compare `on` snd) randomTuples where
    randomTuples = zip l (randomRs (0, 1::Double) (mkStdGen seed))

-- | without replacement
randomSamples :: Seed -> Int -> [a] -> [[a]]
randomSamples seed n dat = map (V.toList . V.backpermute vdat . V.fromList) goodsubsets
  where

    randomIndices = randomRs (0, length dat -1) (mkStdGen seed)
    goodsubsets = filter unique $ chunksOf n randomIndices

    vdat = V.fromList dat

    unique = g . sort
    g []  = True
    g [_] = True
    g (a:b:cs) | a == b    = False
               | otherwise = unique (b:cs)



mean :: (Fractional a) => [a] -> a
mean l = sum l / fromIntegral (length l)

median :: (Ord a) => [a] -> a
median l = sort l !! (div (length l) 2)

-- | minimum, q0.25, q0.5, q.075, maximum
quartiles :: (Ord a) =>  [a] -> (a,a,a,a,a)
quartiles l = (f 0, f a, f b, f c, f n) where
    f = (s!!)
    s = sort l
    n = length l - 1
    n' = fromIntegral n
    [a,b,c] = map (round . (*n')) [0.25,0.5,0.75::Double]

-- | display with name and format the mean, min, max, and quartiles of a list of data
shDist :: String -> String -> String -> [Double] -> IO ()
shDist name fmtm fmt xs = printf (name ++ fmtm ++" ("++fmt++", "++fmt++", "++fmt++", "++fmt++", "++fmt++")\n") m xmin a b c xmax
    where (xmin,a,b,c,xmax) = quartiles xs
          m = mean xs

type Histogram = (V, (V, V))

histogram :: Int -> (ℝ,ℝ) -> Vector Double -> Histogram
histogram n (a,b) xs = (scale s h,(l,r)) 
  where
    nxs = (xs - scalar a) * scalar (fromIntegral n / (b-a))
    idxs = f $ map floor $ toList nxs
    h = accum (konst 0 n) (+) (map (flip (,) 1) idxs)
    z = linspace (n+1) (a,b)
    l = subVector 0 n z
    r = subVector 1 n z
    f = filter (>=0) . filter (<n)
    s = fromIntegral n / fromIntegral (size xs) / (b-a)


meanAndSigma :: Histogram -> (Double, Double, Double)
meanAndSigma (v,(s1,s2)) = (i, m,sqrt (s-m**2)) 
  where
    δ = s2!0-s1!0
    i = sumElements v * δ
    m = (v <·> ((s1+s2)/2)) * δ
    s = (v <·> ((s1+s2)/2)**2) * δ

credible :: Double -> Histogram -> (Double, Double)
credible p (v,(s1,s2)) = (a,b)
  where
    δ = s2!0-s1!0
    a = s1!i
    b = s2!j
    (i,j,_) = until ok go (0,size v -1, 0)
    ok (_,_,x) = x*δ >= (1-p)
    go (l,r,x)
      | v!l < v!r = (l+1,r,x+v!l)
      | otherwise = (l,r-1,x+v!r)

--------------------------------------------------------------------------------

type V = Vector Double

data MCMC = MCMC
    { mcX :: V
    , mcL :: ℝ
    , mcA :: Bool
    }

metropolis :: Seed -> Int -> Int -> ℝ -> V -> Int -> (V -> ℝ) -> ([V],ℝ) 
metropolis seed step burn σ x0 tot lprob = infoMetro $ take tot $ metropolis' seed step burn σ x0 lprob

infoMetro :: [MCMC] -> ([V],ℝ)
infoMetro mcs = (map mcX mcs, ar)
  where
    ar = fromIntegral (length (filter mcA mcs)) / fromIntegral (length mcs)

metropolis' :: Seed -> Int -> Int -> ℝ -> V -> (V -> ℝ) -> [MCMC] 
metropolis' seed step burn σ x0 lprob = g $ scanl f mc0 deltas
  where
    f (MCMC x lp _) (δ,r)
        | accept    = MCMC x' lp' True
        | otherwise = MCMC x  lp  False
      where
        x' = x+δ
        lp' = lprob x'
        ratio = lp' - lp
        accept = ratio > 0 || log r < ratio

    deltas = zip (infn seed (size x0) σ) (infu seed)
    g = drop burn . map head . chunksOf step
    mc0 = MCMC x0 (lprob x0) True


infn :: Int -> Int -> Double -> [V]
infn seed n σ = concatMap g seeds
  where
    seeds = randoms (mkStdGen seed)
    g s = toRows $ gaussianSample s 1000 (vector (replicate n 0)) (trustSym $ diagl (replicate n σ))

infu :: Int -> [ℝ]
infu seed = randomRs (0,1) (mkStdGen seed) :: [ℝ]


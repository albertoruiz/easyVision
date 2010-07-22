-----------------------------------------------------------------------------
{- |
Module      :  Classifier.Neural
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Backpropagation multilayer neural network.

-}
-----------------------------------------------------------------------------

module Classifier.Neural (
     learnNetwork, neural, createNet, adaptNet, perceptron, neural',
) where

import Numeric.LinearAlgebra hiding (eps)
import Classifier.Base
import System.Random
import Util.Misc(norm, randomPermutation)

-------------------------- Multilayer perceptron -----------------------

randomMatrix :: Int -> (Int, Int) -> Matrix Double
randomMatrix seed (n,m) = reshape m $ vector $ take (n*m) $ randomRs (-w,w) $ mkStdGen seed
    where w = 2  / sqrt (fromIntegral m)


data NeuralNet = NN { weights :: [Matrix Double]
                    } deriving Show

createNet :: Int -> Int -> [Int] -> Int -> NeuralNet
createNet seed j hs o = NN ws where
    ws = zipWith randomMatrix [seed, seed+1 .. ] dims
    dims = zip (hs++[o]) ((j+1):hs)

-- the bias constant is only required in the input

-- given a network and an input we obtain the activations of all nodes
forward :: NeuralNet -> Vector Double -> [Vector Double]
forward n v = scanl f (join [v,1]) (weights n)
    where f u m = tanh (m <> u)

-- given a network, activations and desired output it computes the gradient
deltas :: NeuralNet -> [Vector Double] -> Vector Double ->  [Matrix Double]
deltas n xs o = zipWith outer (tail ds) (init xs) where
    dl = (last xs - o) * gp (last xs)
    ds = scanr f dl (zip xs (weights n))
    f (x,m) d = gp x * (trans m <> d)
    gp = mapVector $ \x -> (1+x)*(1-x)

updateNetwork :: Double -> NeuralNet -> (Vector Double, Vector Double) -> NeuralNet
updateNetwork alpha n (v,o) = n {weights = zipWith (+) (weights n) corr}
    where xs = forward n v
          ds = deltas n xs o
          corr = map (scale (-alpha)) ds

epoch :: Double -> NeuralNet -> [(Vector Double, Vector Double)] -> NeuralNet
epoch alpha n prob = foldl (updateNetwork alpha) n prob

backprop :: Double -> NeuralNet -> [(Vector Double, Vector Double)] -> [NeuralNet]
backprop alpha n prob = scanl (epoch alpha) n (repeat prob)

learnNetwork :: Double -> Double -> Int
             -> NeuralNet -> [(Vector Double, Vector Double)] -> (NeuralNet, [Double])
learnNetwork alpha eps maxepochs n prob = (r,e) where
    nets = backprop alpha n prob
    errs = map ((flip mseerror) prob) nets
    evol = zip nets errs
    step = 1 --  *length prob
    selected = [evol!!(k*step) | k <- [0 .. maxepochs]]
    conv = takeWhile (\(_r,e')-> {-debug-} e' >eps) selected
    r = fst$ last$ conv
    e = map (snd) conv

adaptNet :: Sample a -> [(a, Vector Double)]
adaptNet s = x where
    lbs = snd (group s)
    c = length (labels lbs)
    des k c' = vector$ replicate (k-1) (-1) ++ [1] ++ replicate (c'-k) (-1)
    x = [(v, des (1+getIndex lbs l) c) | (v,l) <- s]

-- | multilayer perceptron with alpha parameter and desired number of units in the hidden layers
neural     :: Double -- ^ alpha (e.g. 0.1)
           -> Double -- ^ maxerr (e.g. 0.05)
           -> Int    -- ^ maxepochs (100-1000) 
           -> [Int]  -- ^ hidden units
           -> Learner (Vector Double)
neural alpha eps maxep hidden prob = (c,f) where
    (result, _err) = neural' alpha eps maxep hidden prob
    c = createClassifier (snd$ group prob) f
    f = toList . last . forward result

-- | useful for monitorization of convergence
neural' :: Double -> Double -> Int -> [Int]
        -> Sample (Vector Double)
        -> (NeuralNet, [Double])
neural' alpha eps mxep hidden prob = (network, err) where
    prob' = adaptNet prob
    n = dim$ fst$ head prob'
    m = dim$ snd$ head prob'
    initial = createNet 100 n hidden m
    (network, err) = learnNetwork alpha eps mxep initial prob'

mseerror :: NeuralNet -> [(Vector Double, Vector Double)] -> Double
mseerror r p = sum (map f p) / fromIntegral ( (dim$ snd$ head$ p) * length p)
    where f (x,y) = norm (last (forward r x) - y)

-- | useful for metaalgorithms (treeOf, adaboost)
perceptron :: Double -- ^ alpha (e.g. 0.1)
           -> Double -- ^ maxerr (e.g. 0.05)
           -> Int    -- ^ maxepochs (100-1000) 
           -> [Int]  -- ^ hidden units
           -> Dicotomizer
perceptron alpha eps maxep hidden (g1,g2) = f where
    b = replicate (length g1) 1 ++ replicate (length g2) (-1)
    exs = randomPermutation 1001 (zip (g1++g2) b)
    initial = createNet 100 (dim$ head$ g1) hidden 1
    (result, _err) = learnNetwork alpha eps maxep initial exs
    f = (@>0) . last . forward result

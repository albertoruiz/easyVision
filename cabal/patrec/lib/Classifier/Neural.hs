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
     learnNetwork, neural, createNet, perceptron, neural',
) where

import Numeric.LinearAlgebra hiding (i,eps)
import Classifier.Base
import Util.Misc(norm, randomPermutation, Vec, Mat, Seed)
import Util.Probability(weighted)

-------------------------- Multilayer perceptron -----------------------

randomMatrix :: Seed -> (Int, Int) -> Matrix Double
randomMatrix seed (n,m) = reshape m $ scalar w * randomVector seed Gaussian (n*m)
    where w = 2  / sqrt (fromIntegral m)


data NeuralNet = NN { weights :: [Mat]
                    } deriving Show

createNet :: Seed -> Int -> [Int] -> Int -> NeuralNet
createNet seed i hs o = NN ws where
    ws = zipWith randomMatrix [seed, seed+1 .. ] dims
    dims = zip (hs++[o]) ((i+1):hs)

-- the bias constant is only required in the input

-- given a network and an input we obtain the activations of all nodes
forward :: NeuralNet -> Vec -> [Vec]
forward n v = scanl f (join [v,1]) (weights n)
    where f w m = tanh (m <> w)

-- given a network, activations and desired output it computes the gradient
deltas :: NeuralNet -> [Vec] -> Vec ->  [Mat]
deltas n xs o = zipWith outer (tail ds) (init xs) where
    dl = (last xs - o) * gp (last xs)
    ds = scanr f dl (zip xs (weights n))
    f (x,m) d = gp x * (trans m <> d)
    gp = mapVector $ \x -> (1+x)*(1-x)

updateNetwork :: Double -> NeuralNet -> (Vec, Vec) -> NeuralNet
updateNetwork alpha n (v,o) = n {weights = zipWith (+) (weights n) corr}
    where xs = forward n v
          ds = deltas n xs o
          corr = map (scale (-alpha)) ds

epoch :: Double -> NeuralNet -> [(Vec, Vec)] -> NeuralNet
epoch alpha n prob = foldl (updateNetwork alpha) n prob

backprop :: Double -> NeuralNet -> [(Vec, Vec)] -> [NeuralNet]
backprop alpha n prob = scanl (epoch alpha) n (repeat prob)

learnNetwork :: Double -> Double -> Int
             -> NeuralNet -> [(Vec, Vec)]
             -> (NeuralNet, [Double])
learnNetwork alpha eps maxepochs n prob = (r,e) where
    nets = backprop alpha n prob
    errs = map ((flip mseerror) prob) nets
    evol = zip nets errs
    step' = 1 --  *length prob
    selected = [evol!!(k*step') | k <- [0 .. maxepochs]]
    conv = takeWhile ((>eps).snd) selected
    r = fst $ last $ conv
    e = map (snd) conv


-- | multilayer perceptron with alpha parameter and desired number of units in the hidden layers (to do: use seed)
neural     :: Double -- ^ alpha (e.g. 0.1)
           -> Double -- ^ maxerr (e.g. 0.05)
           -> Int    -- ^ maxepochs (100-1000) 
           -> [Int]  -- ^ hidden units
           -> Learner Vec
neural alpha eps maxep hidden prob = c where
    (result, _err) = neural' alpha eps maxep hidden 100 prob
    f = toList . last . forward result
    (_,lbs) = group prob
    c x = weighted $ zip (labels lbs) (map exp (f x))

-- | useful for monitorization of convergence
neural' :: Double -> Double -> Int
        -> [Int] -> Seed
        -> Sample (Vector Double)
        -> (NeuralNet, [Double])
neural' alpha eps mxep hidden seed prob = (network, err) where
    prob' = vectorLabels prob
    n = dim $ fst $ head prob'
    m = dim $ snd $ head prob'
    initial = createNet seed n hidden m
    (network, err) = learnNetwork alpha eps mxep initial prob'

mseerror :: NeuralNet -> [(Vec, Vec)] -> Double
mseerror r p = sum (map f p) / fromIntegral ( (dim$ snd$ head$ p) * length p)
    where f (x,y) = norm (last (forward r x) - y)

-- | useful for metaalgorithms (to do: use seed)
perceptron :: Double -- ^ alpha (e.g. 0.1)
           -> Double -- ^ maxerr (e.g. 0.05)
           -> Int    -- ^ maxepochs (100-1000) 
           -> [Int]  -- ^ hidden units
           -> Dicotomizer
perceptron alpha eps maxep hidden (g1,g2) = f where
    exs = randomPermutation 1001 (zip (g1++g2) (replicate (length g1) 1 ++ replicate (length g2) (-1)))
    initial = createNet 100 (dim$ head$ g1) hidden 1
    (result, _err) = learnNetwork alpha eps maxep initial exs
    f = (@>0) . last . forward result

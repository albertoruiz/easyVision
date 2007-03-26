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

import GSL
import Classifier.Base
import System.Random

-------------------------- Multilayer perceptron -----------------------

randomMatrix seed (n,m) = reshape m $ vector $ take (n*m) $ randomRs (-w,w) $ mkStdGen seed
    where w = 2  / sqrt (fromIntegral m)


data NeuralNet = NN { weights :: [Matrix Double]
                    } deriving Show

--createNet :: Int -> Int -> [Int] -> Int -> NeuralNet
createNet seed i hs o = NN ws where
    ws = zipWith randomMatrix [seed, seed+1 .. ] dims
    dims = zip (hs++[o]) ((i+1):hs)

-- the bias constant is only required in the input

-- given a network and an input we obtain the activations of all nodes
forward :: NeuralNet -> Vector Double -> [Vector Double]
forward n v = scanl f (join [v,1]) (weights n)
    where f v m = tanh (m <> v)

-- given a network, activations and desired output it computes the gradient
deltas :: NeuralNet -> [Vector Double] -> Vector Double ->  [Matrix Double]
deltas n xs o = zipWith outer (tail ds) (init xs) where
    dl = (last xs - o) * gp (last xs)
    ds = scanr f dl (zip xs (weights n))
    f (x,m) d = gp x * (trans m <> d)
    gp = gmap $ \x -> (1+x)*(1-x)

updateNetwork alpha n (v,o) = n {weights = zipWith (+) (weights n) corr}
    where xs = forward n v
          ds = deltas n xs o
          corr = map (scale (-alpha)) ds

epoch alpha n prob = foldl (updateNetwork alpha) n prob

backprop alpha n prob = scanl (epoch alpha) n (repeat prob)

learnNetwork alpha eps maxepochs n prob = (r,e) where
    nets = backprop alpha n prob
    errs = map ((flip mseerror) prob) nets
    evol = zip nets errs
    step = 1 --  *length prob
    selected = [evol!!(k*step) | k <- [0 .. maxepochs]]
    conv = takeWhile (\(r,e)-> {-debug-} e >eps) selected
    r = fst$ last$ conv
    e = map (snd) conv

--adaptNet :: Sample -> [(Vector Double, Vector Double)]
adaptNet s = x where
    lbs = snd (group s)
    c = length (labels lbs)
    des k c = vector$ replicate (k-1) (-1) ++ [1] ++ replicate (c-k) (-1)
    x = [(v, des (1+getIndex lbs l) c) | (v,l) <- s]

-- | multilayer perceptron with alpha parameter and desired number of units in the hidden layers
neural     :: Double -- ^ alpha (e.g. 0.1)
           -> Double -- ^ maxerr (e.g. 0.05)
           -> Int    -- ^ maxepochs (100-1000) 
           -> [Int]  -- ^ hidden units
           -> Learner
neural alpha eps maxep hidden prob = (c,f) where
    (result, err) = neural' alpha eps maxep hidden prob
    c = createClassifier (snd$ group prob) f
    f = toList . last . forward result

-- | useful for monitorization of convergence
neural' alpha eps mxep hidden prob = (network, err) where
    prob' = adaptNet prob
    n = size$ fst$ head prob'
    m = size$ snd$ head prob'
    initial = createNet 100 n hidden m
    (network, err) = learnNetwork alpha eps mxep initial prob'


mseerror r p = sum (map f p) / fromIntegral ( (size$ snd$ head$ p) * length p)
    where f (x,y) = norm (last (forward r x) - y)

-- | useful for metaalgorithms (treeOf, adaboost)
perceptron :: Double -- ^ alpha (e.g. 0.1)
           -> Double -- ^ maxerr (e.g. 0.05)
           -> Int    -- ^ maxepochs (100-1000) 
           -> [Int]  -- ^ hidden units
           -> Dicotomizer
perceptron alpha eps maxep hidden (g1,g2) = f where
    b = (replicate (length g1) 1 ++ replicate (length g2) (-1))
    exs = scramble 1001 (zip (g1++g2) (replicate (length g1) 1 ++ replicate (length g2) (-1)))
    initial = createNet 100 (size$ head$ g1) hidden 1
    (result, err) = learnNetwork alpha eps maxep initial exs
    f = (@>0) . last . forward result

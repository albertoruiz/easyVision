import Classifier
import Classifier.ToyProblems
import Util.Stat
import Classifier.Regression(msError)
import Util.ICA
import Util.Statistics(randomPermutation)
import Util.Gaussian(mixturePDF,findMixture)

import Numeric.LinearAlgebra
import Vision.GUI.Simple
import Data.Maybe(maybe)
import System.Random(randomIO)
import Text.Printf(printf)
import Control.Monad((>=>))
import Image(Size(..),mat2img)

---------------------------------------------------------------------------

main = do
    demoQuality moon
    democlas sshape

---------------------------------------------------------------------------


scatterPlots name exs mets = browser name xs (const id)
  where
    xs = map f mets
    f (met, name) = scatter exs (0,1) [] (windowTitle name $ drawDecisionRegion 71 exs [] met)


---------------------------------------------------------------------------

rej evi = maybe "REJECT" id . reject evi


democlas x = do
    seed <- randomIO
    let p = addNoise seed 0.1 $ x 500
    runIt $ scatterPlots "" p
        [ (mode   . minDistance euclidean p,   "min euc dist")
        , (rej 1  . minDistance euclidean p,   "min euc dist, 1db")
        , (mode   . minDistance mahalanobis p, "Mahalanobis dist")
        , (mode   . bayes gaussian p,          "gaussian model")
        , (rej 3  . bayes gaussian p,          "gaussian, 3db")
        , (mode   . bayes naiveGaussian p,     "naive gaussian")
        , (rej 15 . bayes gmm p,               "gaussian mixture, 15db")
        , (mode   . minDistance nearestNeighbour p, "nearest neighbour")
        , (rej 1  . lsc p,                     "linear, 1db")
        , (rej 3  . neural 0.1 0.05 100 [] p, "neuron 3db")
        , (rej 3  . neural 0.1 0.05 100 [5] p, "NN [5], 3db")
        , (rej 5  . neural 0.05 0.05 200 [20,10,5] p, "NN [20,10,5], 5db")
        , (mode   . multiclass (adaboost 100 stumps) p, "ADABOOST 100 stumps")
        , (mode   . multiclass (treeOf (branch 3) (dicodist euclidean)) p, "tree of euc dist")
        , (mode   . multiclass (treeOf (branch 5) (perceptron 0.1 0.05 100 [])) p, "tree of neuron")
        ]

----------------------------------------------------------------------


study msg prob meth pred = do
    let (train,test) = prob
        c = meth train
    putStr $ msg ++ "\nTraining error: "
    printf "%.2f %%\n" $ errorRate $ quality train (pred.c)
    putStr "Test quality: "
    shQuality $ quality test (pred.c)


demoQuality x = do
    seed <- randomIO
    let p = splitProportion 0.5 $ randomPermutation seed $ (addNoise seed 0.1 $ x 500)
    study "Euclidean Distance" p (minDistance euclidean) (Just . mode)
    study "LSC 1db" p (lsc) (reject 1)
    study "Mahalanobis Distance" p (minDistance mahalanobis) (Just . mode)
    study "Gaussian" p (bayes gaussian) (Just . mode)
    study "Gaussian 3db" p (bayes gaussian) (reject 3)
    study "Naive Gaussian" p (bayes naiveGaussian) (Just . mode)
    study "Gaussian Mixture 15db" p (bayes gmm) (reject 15)
    study "Multilayer Perceptron [5] 3db" p (neural 0.1 0.05 100 [10]) (reject 3)
    study "Multilayer Perceptron [20,10,5] 5db" p (neural 0.05 0.05 200 [20,10,5]) (reject 5)



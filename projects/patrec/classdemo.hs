{- comparison of different learning machines on a 2D toy problem
-}

import Numeric.LinearAlgebra
import Classifier
import Classifier.ToyProblems
import System.Random
import Util.Misc(randomPermutation)

shErr d c = (show $ (/100) $ fromIntegral $ round $ 10000 * errorRate d c) ++ " %"
shConf d c = putStrLn $ format " " (show.round) (confusion d c)

study :: Sample (Vector Double) -> (Learner (Vector Double), String) -> IO ()
study prob (meth, title) = do
    seed <- randomIO
    let (train,test) = splitProportion 0.5 $ randomPermutation seed prob
    let (c,f) = meth train
    putStrLn title
    let e = shErr test c
    putStrLn $ "Test error: " ++ e
    shConf test c
    putStrLn $ "Training error: " ++ shErr train c
    shConf train c
    putStrLn "-----------------"
    combined (title++"   ["++ e ++"]") 100 2.5 (fromIntegral.posMax.f) test



(r,err) = learnNetwork 0.1 0.05 100 (createNet 100 2 [10,20,10] 1)  (adaptNet xor)


xor :: Sample (Vector Double)
xor = [
 (vector [-1,-1], "b"),
 (vector [-1,1], "a"),
 (vector [1,-1], "a"),
 (vector [1,1], "b")
 ]


machines = [ (distance ordinary, "ordinary distance")
           , (distance gaussian, "gaussian distance")
           , (distance nearestNeighbour, "nearest Neigbour")
           , (multiclass mse, "linear mse")
           , (multiclass (treeOf (branch 0) (unweight stumps)), "tree of stumps")
           , (multiclass (adaboost 50 stumps), "adaboost 50 stumps")
           , (neural 0.1 0.05 100 [10], "neural 10")
           , (neural 0.05 0.05 100 [20,10,5], "neural 20 10 5")
           , (multiclass (svmLight 1.0), "svm-Light gaussian 1.0")
           , (multiclass (kernelMSE 1 (polyK 2)), "kernel mse poly 2")
           , (multiclass (kernelMSE 1 (polyK 5)), "kernel mse poly 5")
           , (multiclass (kernelMSE 1 (gaussK 0.2)), "kernel mse gaussK 0.2")
           , (multiclass $ adaboost 10 $ weight 17 ( treeOf (branch 5) (perceptron 0.1 0.1 10 [2])),"combination")
           ]

problem = addNoise 0 0.1 (sshape 500) -- rings 500

main = mapM_ (study problem) machines

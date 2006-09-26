import Vision
import GSL
import System.Random
import Debug.Trace
import Data.List(inits)
import Control.Monad(when)

debug x = trace (show x) x
debug' msg x = trace (msg ++ show x) x

selectClasses :: [Label] -> Sample -> Sample
selectClasses validset exs = filter ( (`elem` validset) .snd) exs where

----------------------------------------------------------------------

main = do
    let prob = sshape 2000
    -- let method = mse
    -- let method = distance mahalanobis
    -- let method = distance ordinary
    -- let method = singlestump
    let method = multiclass (adaboost 40 stumps)

    study prob method


study prob meth = do
    seed <- randomIO
    let (train,test) = splitProportion 0.5 $ scramble seed prob
    let (c,f) = meth train
    putStr "Test error: "
    print (errorRate test c)
    print (confusion test c)
    putStr "Training error: "
    print (errorRate train c)
    print (confusion train c)
    combined 100 2.5 (fromIntegral.posMax.f) train

--------------------------------------------------------------------------------

testMNIST = do
    (train, test) <- mnist 20 4000
    let (c,f) = mse train
    print (errorRate test c)
    print (confusion test c)
    let (c,f) = distance ordinary train
    print (errorRate test c)
    print (confusion test c)
    let (c,f) = distance mahalanobis train
    print (errorRate test c)
    print (confusion test c)
    let (c,f) = distance mahalanobis' train
    print (errorRate test c)
    print (confusion test c)
    let (c,f) = classstumps 10 train
    print (errorRate test c)
    print (confusion test c)
    let (c,f) = classstumps 60 train
    print (errorRate test c)
    print (confusion test c)

-- comparison of adaboost with other methods
adamnist = do
    (train', test') <- mnist 20 4000
    --(train', test') <- mnistraw 4000
    let sel = ["9","7"]
    let (train,test) = (selectClasses sel train', selectClasses sel test')
    putStrLn "ordinary distance"
    let (c,f) = distance ordinary train
    print (errorRate train c)
    print (errorRate test c)
    print (confusion test c)
    putStrLn "mahalanobis distance"
    let (c,f) = distance mahalanobis' train
    print (errorRate train c)
    print (errorRate test c)
    print (confusion test c)
    putStrLn "adaboost 20"
    let (c,f) = classstumps 20 train
    print (errorRate train c)
    print (errorRate test c)
    print (confusion test c)
    putStrLn "adaboost 100"
    let (c,f) = classstumps 100 train
    print (errorRate train c)
    print (errorRate test c)
    print (confusion test c)



-- to show the learning curve of adaboost
pruada n = do
    seed <- randomIO
    let (train,test) = splitProportion 0.5 $ scramble seed $ sshape 2000
    let ([g1,g2],lbs) = group train
    --let st = adaboostST n mseBinaryWeighted (g1,g2)
    --let st = adaboostST n distWeighted (g1,g2)
    let st = adaboostST n stumps (g1,g2)
    mapM_ (\(_,d,e,a)-> do {print (e,a)}) st
    --mapM_ (\(f,_,_,_)-> combined 50 2.5 (signum.f) [g1,g2]) st
    --combined 100 2.5 (signum.f'') [g1,g2]

    let cc = (\x->[x,-x])
    let combis = map (createClassifier lbs . (cc.) . adaFun)  (tail $ inits st)
    let e1 = map ( (*100). errorRate train) combis
    let e2 = map ( (*100). errorRate test) combis
    mplot [vector [1 .. fromIntegral n], vector e1, vector e2]

    let comb = adaFun st
    combined 50 2.5 comb train
    combined 100 2.5 (signum.comb) train
    let c = last combis
    print (errorRate train c)
    print (errorRate test c)
    print (confusion test c)

-- adaboost learning curves

prucurves 1 = do
    seed <- randomIO
    (train', test') <- mnistraw 4000
    let sel = ["9","7"]
    let (train,test) = (breakTies seed 0.001 (selectClasses sel train'),
                                                selectClasses sel test')
    adaboostCurves 100 stumps (train,test)

prucurves 2 = do
    seed <- randomIO
    let prob = splitProportion 0.5 $ scramble seed $ sshape 2000
    adaboostCurves 30 stumps prob


adaboostCurves :: Int -> WeightedDicotomizer -> (Sample,Sample) -> IO ()
adaboostCurves n method (train, test)= do
    let ([g1,g2],lbs) = group (train)
    let st = adaboostST n stumps (g1,g2)
    mapM_ (\(_,d,e,a)-> do {print (e,a)}) st

    let cc = (\x->[x,-x])
    let combis = map (createClassifier lbs . (cc.) . adaFun)  (tail $ inits st)

    let c = last combis
    print (errorRate train c)
    print (errorRate test c)
    print (confusion test c)

    let e1 = map ( (*100). errorRate train) combis
    let e2 = map ( (*100). errorRate test) combis
    when (length combis > 1 ) $ do
        mplot [vector [1 .. fromIntegral n], vector e1, vector e2]


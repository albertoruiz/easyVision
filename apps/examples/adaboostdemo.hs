import Vision
import GSL
import System.Random
import Debug.Trace
import Data.List(inits, sort, partition)
import Control.Monad(when)

matrix m = fromLists m :: Matrix Double
vector v = fromList v :: Vector Double
vmap = gmap

debug x = trace (show x) x
debug' msg x = trace (msg ++ show x) x


-- | a learner which adaboosts the stumps weak learner
classstumps :: Int -> Learner
classstumps n = multiclass (adaboost n stumps)


----------------------------------------------------------------------

-- | Naive Bayes with gaussian pdf
nbayesg :: Double -> [Vector Double] -> Vector Double -> Double
nbayesg eps vs = f where
    x = fromRows vs
    m = meanVector (stat x)
    m2 = meanVector (stat (x*x))
    s = sqrt (m2 - m*m)
    maxv = maximum (toList s)
    tol = eps * maxv
    s' = vmap pr s
    d = vmap pr' s
    pr x = if x < tol then maxv else x
    pr' x = if x < tol then 0 else 1
    k = sum (map log (toList s'))
    f v = k + 0.5*(norm (d*(v-m)/s'))^2




main = adamnistraw

main' = do
    let prob = sshape 2000
    -- let method = multiclass mse
    -- let method = distance mahalanobis
    -- let method = distance ordinary
    -- let method = singlestump
    let method = multiclass (adaboost 40 stumps)

    study prob method


-- 6 levels of function combinators!
tremen :: Learner
tremen = multiclass $ adaboost 50 $ weight 117 $ treeOf (branch 20) (unweight stumps)


pruprob = zip [vector [k,0] | k <- [0,0.1 .. ]] ["a","a","a","a","a","b","b","a","a","a"]

pru = do
    seed <- randomIO
    let prob = (breakTies seed 0.001 pruprob)
    let (c,f) = classstumps 1 prob
    print (errorRate prob c)
    dispR 2 (confusion prob c)
    print (map (posMax.f.fst) prob)
    print (map (c.fst) prob)
    combined 100 1 (fromIntegral.posMax.f) prob

shErr d c = putStrLn $ (show $ 100 * errorRate d c) ++ " %"
shConf d c = putStrLn $ format " " (show.round) (confusion d c)

study :: Sample -> Learner -> IO ()
study prob meth = do
    seed <- randomIO
    let (train,test) = splitProportion 0.5 $ scramble seed prob
    let (c,f) = meth train
    putStr "Test error: "
    shErr test c
    shConf test c
    putStr "Training error: "
    shErr train c
    shConf train c
    combined 100 2.5 (fromIntegral.posMax.f) train

--------------------------------------------------------------------------------

testMNIST = do
    (train, test) <- mnist 20 4000
    let (c,f) = multiclass mse train
    print (errorRate test c)
    dispR 2 (confusion test c)
    let (c,f) = distance ordinary train
    print (errorRate test c)
    dispR 2 (confusion test c)
    let (c,f) = distance mahalanobis train
    print (errorRate test c)
    dispR 2 (confusion test c)
    let (c,f) = distance mahalanobis' train
    print (errorRate test c)
    dispR 2 (confusion test c)
    let (c,f) = classstumps 10 train
    print (errorRate test c)
    dispR 2 (confusion test c)
    let (c,f) = classstumps 60 train
    print (errorRate test c)
    dispR 2 (confusion test c)

-- comparison of adaboost with other methods
adamnist = do
    (train', test') <- mnist 20 4000
    --(train', test') <- mnistraw 4000
    let sel = ["9","7","4"]
    let (train,test) = (selectClasses sel train', selectClasses sel test')
    putStrLn "ordinary distance"
    let (c,f) = distance ordinary train
    print (errorRate train c)
    print (errorRate test c)
    dispR 2 (confusion test c)
    putStrLn "mahalanobis distance"
    let (c,f) = distance mahalanobis' train
    print (errorRate train c)
    print (errorRate test c)
    dispR 2 (confusion test c)
    putStrLn "adaboost 20"
    let (c,f) = classstumps 20 train
    print (errorRate train c)
    print (errorRate test c)
    dispR 2 (confusion test c)
    putStrLn "adaboost 100"
    let (c,f) = classstumps 100 train
    print (errorRate train c)
    print (errorRate test c)
    dispR 2 (confusion test c)

adamnistraw = do
    (train', test') <- mnistraw 4000
    let sel = ["9","7","4"]
    let (train,test) = (breakTies 100 0.001 $ selectClasses sel train', selectClasses sel test')
    putStrLn "ordinary distance"
    let (c,f) = distance ordinary train
    print (errorRate train c)
    print (errorRate test c)
    dispR 2 (confusion test c)
    when False $ do
        putStrLn "adaboost 20"
        let (c,f) = classstumps 20 train
        print (errorRate train c)
        print (errorRate test c)
        dispR 2 (confusion test c)
        putStrLn "adaboost 100"
        let (c,f) = classstumps 100 train
        print (errorRate train c)
        print (errorRate test c)
        dispR 2 (confusion test c)
        putStrLn "nearest neighbour"
        let (c,f) = (distance closestNeighbour) train
        --print (errorRate train c)
        print (errorRate test c)
        dispR 2 (confusion test c)
        putStrLn "tree of stumps"
        let (c,f) = multiclass (treeOf (branch 20) (unweight stumps)) train
        print (errorRate train c)
        print (errorRate test c)
        dispR 2 (confusion test c)
        putStrLn "mse"
        let (c,f) = multiclass mse train
        print (errorRate train c)
        print (errorRate test c)
        dispR 2 (confusion test c)
        putStrLn "pca mse"
        let (c,f) = withPCA (ReconstructionQuality 0.8) (multiclass mse) train
        print (errorRate train c)
        print (errorRate test c)
        dispR 2 (confusion test c)
        putStrLn "pca kernel mse poly 2"
        let (c,f) = withPCA (ReconstructionQuality 0.8) (multiclass (kernelMSE (polyK 2))) train
        print (errorRate train c)
        print (errorRate test c)
        dispR 2 (confusion test c)


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
    dispR 2 (confusion test c)

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

prunaive 1 = do
    --seed <- randomIO
    (train', test') <- mnistraw 4000
    let sel = ["9","7"]
    let (train,test) = (selectClasses sel train', selectClasses sel test')
    let c = fst $ distance (nbayesg 1E-6) train
    print (errorRate test c)
    dispR 2 (confusion test c)
    let c = fst $ withPCA (ReconstructionQuality 0.5) (distance (nbayesg 1E-6)) train
    print (errorRate test c)
    dispR 2 (confusion test c)
    let c = fst $ distance (ordinary) train
    print (errorRate test c)
    dispR 2 (confusion test c)

-- mnist requires mahalanobis, features are not independent

prunaive 2 = do
    --seed <- randomIO
    (train, test) <- mnistraw 4000
    let c = fst $ distance (nbayesg 1E-6) train
    print (errorRate test c)
    dispR 2 (confusion test c)
    let c = fst $ withPCA (ReconstructionQuality 0.5) (distance (nbayesg 1E-6)) train
    print (errorRate test c)
    dispR 2 (confusion test c)
    let c = fst $ withPCA (ReconstructionQuality 0.95) (distance (nbayesg 1E-6)) train
    print (errorRate test c)
    dispR 2 (confusion test c)
    let c = fst $ distance (ordinary) train
    print (errorRate test c)
    dispR 2 (confusion test c)
    let c = fst $ classstumps 50 train
    print (errorRate train c)
    print (errorRate test c)
    dispR 2 (confusion test c)



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
    dispR 2 (confusion test c)

    let e1 = map ( (*100). errorRate train) combis
    let e2 = map ( (*100). errorRate test) combis
    when (length combis > 1 ) $ do
        mplot [vector [1 .. fromIntegral n], vector e1, vector e2]


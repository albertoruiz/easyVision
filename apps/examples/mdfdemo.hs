import GSL
import Vision
import System.Random
import Debug.Trace

matrix m = fromLists m :: Matrix Double
vector v = fromList v :: Vector Double

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

-----------------------------------------------------------------------


showDist2d = combined 100 0.1 tonta where tonta = (@>0)

withPCA rq = withPreprocess (mef rq)
withMDF = withPreprocess mdf


study' prob meth = do
    let (train,test) = prob
    let (c,f) = meth train
    putStr "Training error: "
    shErr train c
    --shConf train c
    putStr "Test error: "
    shErr test c
    shConf test c


main = do
    m <- fromFile "mnist.txt" (5000,785)
    let vs = toRows (takeColumns 784 m)
    let ls = map (show.round) $ toList $ flatten $ dropColumns 784 m
    let mnist = zip vs ls
    let (train,test) = splitAt 4000 mnist

    let sel = ["8","0","3"]
    let three = (selectClasses sel train, selectClasses sel test)
    let f = encodeVector $ pca (NewDimension 50) (stat . fromRows . map fst . fst $ three)
    let three' = preprocess f (fst three)
    let g = mdf three'
    showDist2d (preprocess g three')

    let rawproblem = (train,test)
    putStr "dist "
    study' rawproblem (distance ordinary)
    putStr "pca 20 dist "
    study' rawproblem (withPCA (NewDimension 20) $ distance ordinary)
    putStr "mdf pca 20 dist "
    study' rawproblem (withPCA (NewDimension 20) $ withMDF $ distance ordinary)
    putStr "mdf pca .9 dist "
    study' rawproblem (withPCA (ReconstructionQuality 0.9) $ withMDF $ distance ordinary)
    putStr "pca 20 maha "
    study' rawproblem (withPCA (NewDimension 20) $ distance mahalanobis')
    putStr "mdf pca 20 maha "
    study' rawproblem (withPCA (NewDimension 20) $ withMDF $ distance mahalanobis')
    putStr "pca .9 maha "
    study' rawproblem (withPCA (ReconstructionQuality 0.9) $ distance mahalanobis')
    putStr "mdf pca .9 maha "
    study' rawproblem (withPCA (ReconstructionQuality 0.9) $ withMDF $ distance mahalanobis')


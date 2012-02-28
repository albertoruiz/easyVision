import Classifier
import Classifier.ToyProblems
import Util.Stat
import Classifier.Regression(msError)
import Util.ICA
import Util.Misc(debug,randomPermutation)
import Util.Gaussian(mixturePDF,findMixture)

import Numeric.LinearAlgebra
import Vision.GUI
import Data.Maybe(maybe)
import System.Random(randomIO)
import Text.Printf(printf)
import Control.Monad((>=>))
import ImagProc(Size(..),mat2img,resize)

import ScatterPlot

---------------------------------------------------------------------------

rawmnist = loadExamples "../../data/ml/mnist.txt"

-- FIXME separate demos

main = do
    democlas sshape
    demoQuality moon
    scatters (map return "0123")
    testBrowser 20 ["0","1"]
    checkpca

---------------------------------------------------------------------------

scw title p = browser title xs (const id)
  where
    xs = [scatter p (0,1) [] (Draw())]

scatterPlots name exs mets = browser name xs (const id)
  where
    xs = map f mets
    f (met, name) = scatter exs (0,1) [] (windowTitle name $ drawDecisionRegion 71 exs [] met)

scw3 name ps = browser3D name xs (const id)
  where
    xs = map (\p-> scatter3D p (0,1,2) [] (Draw())) ps

---------------------------------------------------------------------------

rej evi = maybe "REJECT" id . reject evi


democlas x = do
    seed <- randomIO
    let p = addNoise seed 0.1 $ x 500
    runIt $ scatterPlots "" p
        [ (mode   . minDistance euclidean p,   "min euc dist")
        , (rej 1  . minDistance euclidean p,   "min euc dist, 1db")
        , (rej 1  . lsc p,                     "lsc, 1db")
        , (mode   . minDistance mahalanobis p, "Mahalanobis dist")
        , (mode   . bayes gaussian p,          "gaussian model")
        , (rej 3  . bayes gaussian p,          "gaussian, 3db")
        , (mode   . bayes naiveGaussian p,     "naive gaussian")
        , (rej 15 . bayes gmm p,               "gaussian mixture, 15db")
        , (rej 3 .  neural 0.1 0.05 100 [5] p, "NN [5], 3db")
        , (rej 5 .  neural 0.05 0.05 200 [20,10,5] p, "NN [20,10,5], 5db")
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


---------------------------------------------------------------------------
-- check PCA reconstruction quality measured in sigma units

checkpca = do
    x <- map (fst) `fmap` rawmnist
    let xm = fromRows x
        st = stat xm
        xx = zip x x
        x0 = meanVector st
        codec = pca (SigmaPercent 90) st
        f = decodeVector codec . encodeVector codec
        e0 = msError (const x0) xx
        e = msError f xx
        cov = covarianceMatrix st
    printf "sigma0 cov: %7.3f\n"  $ sqrt $ (sumElements $ takeDiag cov) / fromIntegral (cols xm)
    printf "sigma0:     %7.3f\n" e0
    printf "sigma rec:  %7.3f\n" e
    printf "%.2f %% sigma - " $ 100 - 100*e/e0
    printf "%.2f %% var\n" $ 100 - 100*(e/e0)^2

---------------------------------------------------------------------------

shDigRaw v = Draw . resize (Size 200 200).  mat2img . single $ (reshape 28 v)

shDig = shDigRaw . autoscale

showImage name img = browser name [Draw img] (const id)

autoscale v = (v - scalar b) / scalar (a-b)
  where a = maxElement v
        b = minElement v

testBrowser n c = runIt $ do
    mnist <- rawmnist
    let zeros = selectClasses c mnist
    browseLabeled (concat c) zeros shDig
    let x = fromRows $ map fst zeros
        st = stat x
        med = meanVector st
        pc = toRows $ eigenvectors $ st
        aux = map (\a -> (a,""))
    let codec = pca (NewDimension n) st
        red = encodeMatrix codec x
        wd = whitenedData . stat $ red
        w = whitener . stat $ red
        ica = fst . debug "err: " snd . icaTrans 50 (ica2 0.01) $ wd
        icaorig = toRows $ decodeMatrix codec (ica <> inv w) - asRow med
        icab = toRows $ ica <> (takeRows n $ eigenvectors st)
    browseLabeled "IC" (aux icaorig) shDig
    
    let p = preprocess (mef (NewDimension n) zeros) zeros
    scw "PCA" p
    scw3 "PCA" [ boxAttr p `preprocess` p ]
    let pw = aux $ toRows $ wd
    scw3 "Whitened" [boxAttr pw `preprocess` pw]
    let pica = aux $ toRows $ wd <> trans ica
    scw "ICA" pica
    scw3 "ICA" [pica]
    showImage "PCA vs ICA" $ mat2img . single $ fromBlocks $ map (\x->map (reshape 28 . autoscale) (take n x)) [pc,icab]
    let expca k  = expand (fst $ zeros!!k) med (take n pc)
    showImage "Expansion PCA " $ mat2img . single $ autoscale $ fromBlocks $ map (\t->map (reshape 28 ) (take (n+3) t)) (map expca [0..9])
    let exica k  = expand (fst $ zeros!!k) med icab
    showImage "Expansion ICA " $ mat2img .single $ autoscale $ fromBlocks $ map (\t->map (reshape 28 ) (take (n+3) t)) (map exica [0..9])
    --return $ fromRows icab <> trans (fromRows icab)


expand x m bs = x : r : map (scale 1) cs'  where
    ys = map (<.>(x - m)) bs
    cs = zipWith scale ys bs
    r = sum cs'
    n = fromIntegral $ length bs
    m' = scale (recip n) m
    cs' = map (+m')  cs

expand' x m bs = x : r : m : cs where
    ys = map (<.>(x - m)) bs
    cs = zipWith scale ys bs
    r = sum (m : cs)

---------------------------------------------------------

scatters s = runIt $ do
    raw <- rawmnist
    let sel = filter ((`elem` s) . snd) raw
    scw3 "PCA All" [ (boxAttr `ofP` mef (NewDimension 3)) raw `preprocess` raw ]
    scw3 ("PCA " ++ concat s) [ (boxAttr `ofP` mef (NewDimension 3)) sel `preprocess` sel ]

    let redu = mef (NewDimension 40) raw `preprocess` raw
        sel = filter ((`elem` s) . snd) redu
        sep = (boxAttr `ofP` mdf) sel `preprocess` sel
    scw3 "MDF " [sep]

    let dig d =  (boxAttr `ofP` mef (NewDimension 3)) they `preprocess` they
          where they = filter ((`elem` [d]) . snd) raw
    scw3 "PCA each" (map (dig.return) "0123456789")


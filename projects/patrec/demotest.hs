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

import Util.ScatterPlot

---------------------------------------------------------------------------

rawmnist = loadExamples "../../data/ml/mnist.txt"

main = do
    checkpca
    testBrowser 20 ["0","1"]

---------------------------------------------------------------------------

scw title p = browser title xs (const id)
  where
    xs = [scatter p (0,1) [] (Draw())]


scw3 name ps = browser3D name xs (const id)
  where
    xs = map (\p-> scatter3D p (0,1,2) [] (Draw())) ps

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



import EasyVision
import Classifier
import Classifier.Base
import Classifier.ToyProblems
import Graphics.UI.GLUT(mainLoop)
import Numeric.LinearAlgebra
import Text.Printf
import System.Random(randomIO)
import Util.Misc(randomPermutation,Vec)
import Data.Colour.Names as Col
import Classifier.Regression


main = do

    let p = addNoise 0 0.3 $ moon 300

    scwm p (distance mahalanobis)
    scwm p (distance gaussian)
    scwm p (multiclass $ treeOf (branch 0) (dicodist ordinary ) )


    sz <- findSize

    prepare

    --let exs = addNoise 0 0.1 (sshape 500)
    --scatterWindow "Feature Space" (Size 400 400) exs (0,1)

    raw <- rawmnist
    let redu = mef (NewDimension 40) raw `preprocess` raw
        sel = filter ((`elem` ["3","8","6"]) . snd) redu
        sep = mdf sel `preprocess` sel

    scatterPlot "PCA" (Size 400 400) redu (0,1) colors (return ())

    scatterPlot "PDA" (Size 400 400) sep (0,1) [] (return ())

    shQuality sep $ fst (distance mahalanobis sep)
    shQuality sep $ fst (distance ordinary sep)
    shQuality sep $ fst (distance naiveGaussian sep)

    shQuality redu $ fst (distance ordinary redu)
    shQuality redu $ fst (distance naiveGaussian redu)
    shQuality redu $ fst (distance mahalanobis redu)
    shQuality redu $ fst (distance gaussian redu)
    shQuality redu $ fst (distance (subspace (NewDimension 10)) redu)
    mainLoop

colors = [red,blue,green]++repeat Col.lightgray

scw p = do
    prepare
    scatterPlot "Feature Space" (Size 400 400) p (0,1) colors (return ())
    mainLoop

scwc p clasif = do
    prepare
    scatterPlot "Feature Space" (Size 400 400) p (0,1) colors (drawRegion clasif p colors)
    mainLoop

scwm p met = scwc p (fst $ met p)


p = linsepmulti 100
clas = fst $ distance nbg p
vec x = fromList x :: Vector Double

shQuality d c = do
    let m = confusion d c
    printf "%.2f %%\n" (100 - 100 * sumElements (takeDiag m) / sumElements m)
    putStrLn $ format " " (show.round) m

study met prob = do
    seed <- randomIO
    let (train,test) = splitProportion (3/4) (randomPermutation seed prob)
        clas = (fst.met) train
    printf "(%.1f %%) " $ 100 * errorRate train clas
    shQuality test clas


nbg vs = f where
    x = fromRows vs
    m = debug $ meanVector (stat x)
    m2 = meanVector (stat (x*x))
    s = debug $ sqrt (m2 - m*m)
    k = sumElements (log s)
    norm x = pnorm PNorm2 x
    f v = k + 0.5*norm (debug $ (v-m)/s) ^2

rawmnist = do
    m <- fromFile "../../data/mnist.txt" (5000,785)
    let vs = toRows (takeColumns 784 m)
        ls = map (show.round) $ toList $ flatten $ dropColumns 784 m
    return $ zip vs ls

dicodist :: Distance Vec -> Dicotomizer
dicodist d (g1,g2) = f where
    [d1,d2] = map d [g1,g2]
    f x = d2 x - d1 x

---------------------------------------------------------

checkpca = do
    x <- map (toList.fst) `fmap` rawmnist
    let xm = fromLists x
        st = stat xm
        xx = zip x x
        x0 = meanVector st
        codec = pca (ReconstructionQuality 0.1) st
--        codec = pca (NewDimension 10) st
        f = decodeVector codec . encodeVector codec
        e0 = msError (const x0) xx
        e = msError f xx
        cov = covarianceMatrix st
    print $ sqrt $ (sumElements $ takeDiag cov) / fromIntegral (cols xm)
    print e0
    print e
    printf "%.2f %% sigma - " $ 100*e/e0
    printf "%.2f %% var\n" $ 100*(e/e0)^2

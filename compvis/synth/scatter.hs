import EasyVision hiding (debug)
import Classifier
import Classifier.Base
import Util.Probability
import Classifier.ToyProblems
import Graphics.UI.GLUT(mainLoop)
import Numeric.LinearAlgebra
import Text.Printf
import System.Random(randomIO)
import Util.Misc(randomPermutation,Vec,vec,debug)
import Data.Colour.Names as Col
import Classifier.Regression
import qualified Data.List as L
import Data.Array

printev prob = printf "%s : %.1f db\n" (show m) evi
    where m = mode prob
          evi = evidence m prob

distanceH :: Distance a -> Learner a
distanceH ds exs = c where
    (gs,lbs) = group exs
    distfuns = map ds gs
    liks x = heur $ debug "raw liks: " id $ map ($x) distfuns
    c x = weighted $ zip (labels lbs) (liks x)
    heur xs = map f xs
        where t = minimum xs
              f x | x == t    = 1
                  | otherwise = t/x


bayes' ds exs = c where
    (gs,lbs) = group exs
    distfuns = map ds gs
    liks x = debug "scaled liks: " sum $ normSafe (h/10) $ debug "raw liks: " id $ map (w.($x)) distfuns
    w d = exp (-d)
    c x = weighted $ zip (labels lbs) (liks x)
    h = debug "h: " id $ minimum $ debug "brut: " id $ concat $ map ((\x-> map (w.($x)) distfuns).fst) exs

normSafe h ws = if t > h then map (/t) ws else map (const 1) ws
    where t = debug "max: " id $ maximum ws


p = addNoise 777 0.1 (linsepmulti 100)
q = addNoise 0 0.3 $ moon 300
clas = distanceH euclidean p
gclas = distanceH gaussian q
bclas = bayes gaussian q

main = do

    let p = addNoise 0 0.2 $ moon 300

    scwm p (distance mahalanobis)
    scwm p (distance gaussian)
    scwm p (bayes gaussian)
    scwm p (multiclass $ treeOf (branch 0) (dicodist euclidean ) )

    mnistTest mode (bayes gaussian  `onP` mef (NewDimension 20))

    mnistTest (reject 20) (bayes gaussian `onP` mef (NewDimension 40))

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

    scwm' sep (distance euclidean)

    shQuality sep $ mode . bayes euclidean sep
    shQuality sep $ mode . distance euclidean sep
    shQuality sep $ mode . distance naiveGaussian sep

    shQuality redu $ mode . distance euclidean redu
    shQuality redu $ mode . distance naiveGaussian redu
    shQuality redu $ mode . distance mahalanobis redu
    shQuality redu $ mode . bayes gaussian redu
    shQuality redu $ mode . distance (subspace (NewDimension 10)) redu
    mainLoop

colors = [red,blue,lightgreen]++repeat Col.lightgray

scw p = do
    prepare
    scatterPlot "Feature Space" (Size 400 400) p (0,1) colors (return ())
    mainLoop

scwc p clasif = do
    prepare
    scwc' p clasif
    mainLoop

scwc' p clasif = do
    scatterPlot "Feature Space" (Size 400 400) p (0,1) colors (drawRegion clasif p colors)



scwm p met = scwc p (mode . met p)
scwm' p met = scwc' p (mode . met p)

scwme evi p met = scwc p (reject evi . met p)

reject db p = if evidence m p < db
                then "REJECT"
                else m
    where m = mode p


shQuality d c = do
    let m = confusionR d c
        ac = sumElements (takeDiag m)
        rj = sumElements (dropColumns (rows m) m)
        tt = sumElements m
    print (tt,ac,rj)
    printf "Rej: %.2f %%  Err: %.2f\n" (100 * rj / tt) (100 - 100 * ac / (tt-rj))
    putStrLn $ format " " (show.round) m

study ev prob met = do
    seed <- randomIO
    let (train,test) = splitProportion (3/4) (randomPermutation seed prob)
        clas = reject ev . met train
    printf "(%.1f %%) " $ 100 * errorRate train clas
    shQuality test clas


mnistTest dec met = do
    mnist <- rawmnist
    let (train,test) = splitAt 4000 mnist
        clas = dec . met train
    printf "(%.1f %%) " $ 100 * errorRate train clas
    shQuality test clas



rawmnist = loadExamples "../../data/mnist.txt"

dicodist :: Distance Vec -> Dicotomizer
dicodist d (g1,g2) = f where
    [d1,d2] = map d [g1,g2]
    f x = d2 x - d1 x


-- | Constructs a (multiclass) 'Learner given any 'Dicotomizer' (by creating n features to discriminate each class against the rest)
multiclassN :: Dicotomizer -> Property Vec Vec
multiclassN bin exs = f where
    (gs,lbs) = group exs
    f = multiclass' bin gs


multiclass' :: Dicotomizer -> [[Vec]] -> Vec -> Vec
multiclass' _ [] = error "multiclass applied to 0 classes"
multiclass' _ [_] = error "multiclass applied to 1 class"
multiclass' bin [g1,g2] = (\x -> vec [x,-x]) . bin (g1,g2)
multiclass' bin l = f where
    fs = map bin (auxgroup l)
    f v = vec $ map ($v) fs


auxgroup :: [[a]] -> [([a], [a])]
auxgroup l = map (\(x:xs) -> (x, concat xs)) (rots l) where
    rot _ [] = []
    rot 0 xs = xs
    rot k (x:xs) = rot (k-1) (xs++[x])
    rots xs = map ((flip rot) xs) [0 .. length xs - 1]


---------------------------------------------------------

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
    print $ sqrt $ (sumElements $ takeDiag cov) / fromIntegral (cols xm)
    print e0
    print e
    printf "%.2f %% sigma - " $ 100 - 100*e/e0
    printf "%.2f %% var\n" $ 100 - 100*(e/e0)^2


-- | Computes the confusion matrix of a classifier on a sample
confusionR ::Sample a -> (a -> Label) -> Matrix Double
confusionR exs c = confusionMatrix where
    (_,lbs) = group exs
    l = getIndex lbs
    estimatedLabels = map (r l.c.fst) exs
    te = zip (map (l.snd) exs) estimatedLabels
    nc = length (labels lbs)
    confusionMatrix = fromArray2D $
        accumArray (+) (0::Double) ((0::Int,0::Int),(nc-1,nc)) (zip te [1,1 ..])
    r f x | x == "REJECT" = nc
          | otherwise     = f x


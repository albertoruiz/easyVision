import EasyVision hiding (debug, whitener)
import Classifier
import Classifier.Base
import Util.Probability
import Util.Stat
import Util.ICA
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
import Graphics.UI.GLUT hiding (Size)
import Data.Maybe(maybe)

scw3 p = do
    prepare
    scw3' p
    mainLoop

scw3' p = scatterPlot3D "Feature Space" 400 p (0,1,2) colors (return ())

scatter3D = do
    raw <- rawmnist
    let redu = (whitenAttr `ofP` mef (NewDimension 10)) raw `preprocess` raw
    scw3 redu

scatter3D2 = do
    raw <- rawmnist
    let redu = mef (NewDimension 40) raw `preprocess` raw
        sel = filter ((`elem` ["3","8","6","4"]) . snd) redu
        sep = (whitenAttr `ofP` mdf) sel `preprocess` sel
    scw3 sep


scatter3D3 = do
    raw <- rawmnist
    let sel = filter ((`elem` ["3","8","6","4"]) . snd) raw
        redu = (whitenAttr `ofP` mef (NewDimension 3)) sel `preprocess` sel
    scw3 redu

scatter3D4 = do
    raw <- rawmnist
    let sel = filter ((`elem` ["3","8","6","4"]) . snd) raw
        redu = (boxAttr `ofP` mef (NewDimension 3)) sel `preprocess` sel
    scw3 redu


scatters = do
    prepare
    raw <- rawmnist
    let sel = filter ((`elem` ["0","1","2","3"]) . snd) raw
    scw3' $  (boxAttr `ofP` mef (NewDimension 3)) raw `preprocess` raw
    scw3' $  (boxAttr `ofP` mef (NewDimension 3)) sel `preprocess` sel

    let redu = mef (NewDimension 40) raw `preprocess` raw
        sel = filter ((`elem` ["3","8","6","4"]) . snd) redu
        sep = (boxAttr `ofP` mdf) sel `preprocess` sel
    scw3' sep
    let dig d = do
           let they = filter ((`elem` [d]) . snd) raw
           scw3' $  (whitenAttr `ofP` mef (NewDimension 3)) they `preprocess` they

    --mapM_ dig (map return "0123456789")
    mainLoop


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

    scwm p (minDistance mahalanobis)
    scwm p (minDistance gaussian)
    scwm p (bayes gaussian)
    scwm p (multiclass $ treeOf (branch 0) (dicodist euclidean ) )

--    mnistTest mode (bayes gaussian  `onP` mef (NewDimension 20))

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

    scwm' sep (minDistance euclidean)

    shQuality sep $ Just . mode . bayes euclidean sep
    shQuality sep $ Just . mode . minDistance euclidean sep
    shQuality sep $ Just . mode . minDistance naiveGaussian sep

    shQuality redu $ Just . mode . minDistance euclidean redu
    shQuality redu $ Just . mode . minDistance naiveGaussian redu
    shQuality redu $ Just . mode . minDistance mahalanobis redu
    shQuality redu $ Just . mode . bayes gaussian redu
    shQuality redu $ Just . mode . minDistance (subspace (NewDimension 10)) redu
    mainLoop

colors = [red,blue,orange,green]++repeat Col.lightgray

scw p = do
    prepare
    scw' p
    mainLoop

scw' p = scatterPlot "Feature Space" (Size 400 400) p (0,1) colors (return ())


scwc p clasif = do
    prepare
    scwc' p clasif
    mainLoop

scwc' p clasif = do
    scatterPlot "Feature Space" (Size 400 400) p (0,1) colors (drawRegion clasif p colors)



scwm p met = scwc p (mode . met p)
scwm' p met = scwc' p (mode . met p)

scwme evi p met = scwc p (maybe "REJECT" id . reject evi . met p)



study ev prob met = do
    seed <- randomIO
    let (train,test) = splitProportion (3/4) (randomPermutation seed prob)
        clas = reject ev . met train
    --printf "(%.1f %%) " $ 100 * errorRate train clas
    shQuality test clas


mnistTest dec met = do
    mnist <- rawmnist
    let (train,test) = splitAt 4000 mnist
        clas = dec . met train
    --printf "(%.1f %%) " $ 100 * errorRate train clas
    shQuality test clas



rawmnist = loadExamples "../../data/mnist.txt"



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

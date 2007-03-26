-----------------------------------------------------------------------------
{- |
Module      :  Classifier.Base
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Common definitions and functions for pattern classification and machine learning.

-}
-----------------------------------------------------------------------------

module Classifier.Base (
-- * Basic definitions
     Attributes, Label, Example, Sample,
     Classifier, Estimator, Learner,
     Feature, TwoGroups, Dicotomizer, multiclass,
     Weights, WeightedDicotomizer, unweight, weight,
-- * Utilities
     errorRate, confusion, InfoLabels(..), group, ungroup, createClassifier, scramble, addNoise, selectClasses, splitProportion, posMax, preprocess, withPreprocess, partit, vector,
-- * Linear feature extraction
     mef, mdf,
-- * Attribute normalization
     normalizeAttr,
     module Classifier.Stat
) where

import GSL
import Data.List(sortBy, sort, nub, elemIndex, intersperse, transpose, partition, delete)
import qualified Data.Map as Map
import System.Random
import Data.Array
import Classifier.Stat
import Debug.Trace

debug x = trace (show x) x
debug' msg x = trace (msg ++ show x) x

matrix = fromLists :: [[Double]] -> Matrix Double
vector = fromList ::  [Double] -> Vector Double


------------------- General definitions ----------------------------

type Attributes = Vector Double
type Label = String
type Example = (Attributes, String)
type Sample = [Example]

-- | A generic learning machine
type Learner = Sample -> (Classifier, Estimator)

type Classifier = Attributes -> Label     -- crisp decision

-- | More informative 'Classifier', which obtains the relative \"confidence\" of each class (related to an approximation to the posterior probabilities)
type Estimator = Attributes -> [Double]


-- | A function that tries to discriminate between two classes of objects (positive means the first class)
type Feature = Attributes -> Double       -- +/-

type TwoGroups = ([Attributes],[Attributes]) -- +/-

-- | A learning machine for binary classification problems. See 'multiclass'.
type Dicotomizer = TwoGroups -> Feature

type Weights = Vector Double
-- | An improved 'Dicotomizer' which admits a distribution on the given examples.
type WeightedDicotomizer = TwoGroups -> Weights -> Feature


-- | Class labels info with helper functions
data InfoLabels = InfoLabels {
    labels :: [String],
    getIndex :: String -> Int,
    getLabel :: Int -> String
}

-- | extracts the labels of a sample
extractLabels :: Sample -> InfoLabels
extractLabels l = InfoLabels ls lti itl where
    ls = sort $ nub $ map snd l
    itl = (ls!!)
    m = createMap ls
    lti = (Map.!) m
    -- | given a list of labels creates a map from them to their class index
    createMap :: [String] -> Map.Map String Int
    createMap ls = Map.fromList (zip ls [0..])

compareBy f = (\a b-> compare (f a) (f b))

partit _ [] = []
partit n l  = take n l : partit n (drop n l)

-- | pseudorandom permutation of a list
scramble :: Int -> [a] -> [a]
scramble seed l = map fst $ sortBy (compareBy snd) randomTuples where
    randomTuples = zip l (randomRs (0, 1::Double) (mkStdGen seed))

-- | add noise to the attributes
addNoise :: Int -> Double -> Sample -> Sample
addNoise seed sz l = zip rvs lbs where
    (vs,lbs) = unzip l
    n = size (head vs)
    randomVectors = map vector $ partit n $ randomRs (-sz, sz::Double) (mkStdGen seed)
    rvs = zipWith (+) vs randomVectors

-- | obtains training and testing subsamples given the required proportion of training instances. You may need to scramble the original sample.
splitProportion :: Double -> [a] -> ([a],[a])
splitProportion r l = splitAt n l where n = round (r * fromIntegral (length l))

-- | creates a 'Classifier' from an 'Estimator' and the particular class labels of a problem.
createClassifier :: InfoLabels -> Estimator -> Classifier
createClassifier ilbs f = getLabel ilbs . posMax . f

-- | returns the position of the maximum element in a list. TODO: remove irrefutable pattern
posMax :: Ord a => [a] -> Int
posMax l = p where
    Just p = elemIndex (maximum l) l

posMin l = p where
    Just p = elemIndex (minimum l) l


-- | groups the attribute vectors of each class
group :: Sample -> ([[Attributes]], InfoLabels)
group l = (gs, lbs) where
    lbs = extractLabels l
    gs = map proto (labels lbs) where proto lb = [v | (v,s)<-l , s==lb]

-- | converts a list of groups of vectors into a 'Sample' with different labels
ungroup :: [[Attributes]] -> Sample
ungroup gs = s where
    n = length gs
    lbs = map (show) [1 .. n]
    s = concat $ zipWith f gs lbs
    f g lb = zip g (repeat lb)

-- | Estimates the success rate of a classifier on a sample
errorRate :: Sample -> Classifier -> Double
errorRate exs c  = fromIntegral ok / fromIntegral (length exs) where
    ok = length [1 | (v,l)<-exs, l /= c v]

-- | Computes the confusion matrix of a classifier on a sample
confusion ::Sample -> Classifier -> Matrix Double
confusion exs c = confusionMatrix where
    lbs = extractLabels exs
    l = getIndex lbs
    estimatedLabels = map (l.c.fst) exs
    te = zip (map (l.snd) exs) estimatedLabels 
    nc = length (labels lbs)
    confusionMatrix = fromArray2D $ 
        accumArray (+) (0::Double) ((0::Int,0::Int),(nc-1,nc-1)) (zip te [1,1 ..])


-- | Applies some preprocessing function to the Attributes of a Sample
preprocess :: (Vector Double -> Vector Double) -> Sample -> Sample
preprocess f exs = [(f v, l) | (v,l) <- exs]



rot 0 xs = xs
rot k (x:xs) = rot (k-1) (xs++[x])
rots l = map ((flip rot) l) [0 .. length l - 1]
auxgroup l = map (\(x:xs) -> (x, concat xs)) (rots l)

-- auxgroup x = zip x (map (concat . flip delete x) x)


-- | Constructs a (multiclass) 'Learner' given any 'Dicotomizer' (by creating n features to discriminate each class against the rest)
multiclass :: Dicotomizer -> Learner
multiclass bin exs = (createClassifier lbs f, f) where
    (gs,lbs) = group exs
    f = multiclass' bin gs

multiclass' _ [] = error "multiclass applied to 0 classes"
multiclass' _ [_] = error "multiclass applied to 1 class"
multiclass' bin [g1,g2] = (\x -> [x,-x]) . bin (g1,g2)
multiclass' bin l = f where
    fs = map bin (auxgroup l)
    f v = map ($v) fs


-- | selects the examples with the given labels
selectClasses :: [Label] -> Sample -> Sample
selectClasses validset exs = filter ( (`elem` validset) .snd) exs


-- | combines a learner with a given preprocessing stage
withPreprocess :: ([Example] -> (Vector Double -> Vector Double))
                -> Learner -> Learner
withPreprocess method learner prob = (c,f) where
    t = method prob
    prob' = preprocess t prob
    (c',f') = learner prob'
    c = c' . t
    f = f' . t 

mean = meanVector . stat
cov  = covarianceMatrix . stat
meancov x = (m,c) where
    m = meanVector st
    c = covarianceMatrix st
    st = stat x

-- warning: we assume descending order in eigR (!?)
-- | Most discriminant linear features (lda)
mdf :: [Example] -> (Vector Double -> Vector Double)
mdf exs = f where
    f x = (x - m) <> v'
    n = length gs - 1
    gs = fst$ group exs
    (v',_) = fromComplex$ takeColumns n v
    (l,v) = eigR (inv c <> cm)
    (m,c) = meancov$ fromRows$ map fst exs
    cm = cov$ fromRows$ map (mean.fromRows) $ gs

-- | Most expressive linear features (pca)
mef :: PCARequest -> [Example] -> (Vector Double -> Vector Double)
mef rq exs = encodeVector . pca rq . stat . fromRows . map fst $ exs


-- | Attribute normalization
normalizeAttr :: Sample -> (Vector Double -> Vector Double)
normalizeAttr exs = f where
    f x = (x-m)/(1+d)
    xs = fromRows $ map fst exs
    m = mean xs
    d = sqrt (mean (xs*xs) - m*m)



-- | Converts a 'WeightedDicotomizer' into an ordinary 'Dicotomizer' (using an uniform distribution).
unweight :: WeightedDicotomizer -> Dicotomizer
unweight dic (g1,g2) = dic (g1,g2) w where
    m = length g1 + length g2
    w = constant (1/fromIntegral m) m

-- | Converts a 'Dicotomizer' into a 'WeightedDicotomizer' (by resampling).
weight :: Int -- ^ seed of the random sequence
          -> Dicotomizer -> WeightedDicotomizer
weight seed dic (g1,g2) w = dic (g1',g2') where
    s = ungroup [g1,g2]
    ac = scanl1 (+) (toList w)
    rs = take (length ac) $ randomRs (0, 1::Double) (mkStdGen seed)
    rul = zip ac s
    elm pos = snd $ head $ fst $ partition ((>pos).fst) rul
    [g1',g2'] = fst (group (addNoise seed 0.0001 $ map elm rs))

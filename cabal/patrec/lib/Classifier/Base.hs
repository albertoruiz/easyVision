-----------------------------------------------------------------------------
{- |
Module      :  Classifier.Base
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  portable

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
     errorRate, confusion, InfoLabels(..), group, ungroup, createClassifier, addNoise, selectClasses, splitProportion, posMax, vector, detailed,
     module Util.Stat,
-- * Feature extraction combinators
-- $FEATCOMB
     Property, withPreprocess, onP, ofP, andP, outputOf, preprocess,
-- * Linear feature extraction
     mef, mdf,
-- * Attribute normalization
     whitenAttr, normalizeAttr, normalizeMinMax
) where

import Numeric.LinearAlgebra
import Data.List(sortBy, sort, nub, elemIndex, intersperse, transpose, partition, delete)
import qualified Data.Map as Map
import System.Random
import Data.Array
import Util.Stat
import Util.Misc(debug,splitEvery,posMax,randomPermutation)
import Data.Function(on)

matrix = fromLists :: [[Double]] -> Matrix Double
vector = fromList ::  [Double] -> Vector Double

------------------- General definitions ----------------------------

type Label = String
type Example a = (a, Label)
type Attributes = Vector Double
type Sample a = [Example a]

type Classifier a = a -> Label    -- crisp decision

-- | More informative 'Classifier, which obtains the relative \"confidence\" of each class (related to an approximation to the posterior probabilities)
type Estimator a = a -> [Double] -- hmm

-- | A generic learning machine
type Learner a = Sample a -> (Classifier a, Estimator a)

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
extractLabels :: Sample a -> InfoLabels
extractLabels l = InfoLabels ls lti itl where
    ls = sort $ nub $ map snd l
    itl = (ls!!)
    m = createMap ls
    lti = (Map.!) m
    -- | given a list of labels creates a map from them to their class index
    createMap :: [String] -> Map.Map String Int
    createMap ls = Map.fromList (zip ls [0..])

-------------------------------------------------------------------


-- | add noise to the attributes
addNoise :: Int -> Double -> Sample (Vector Double) -> Sample (Vector Double)
addNoise seed sz l = zip rvs lbs where
    (vs,lbs) = unzip l
    n = dim (head vs)
    randomVectors = map vector $ splitEvery n $ randomRs (-sz, sz::Double) (mkStdGen seed)
    rvs = zipWith (+) vs randomVectors

-- | obtains training and testing subsamples given the required proportion of training instances. You may need to scramble the original sample.
splitProportion :: Double -> [a] -> ([a],[a])
splitProportion r l = splitAt n l where n = round (r * fromIntegral (length l))

-- | creates a 'Classifier from an 'Estimator and the particular class labels of a problem.
createClassifier :: InfoLabels -> Estimator a -> Classifier a
createClassifier ilbs f = getLabel ilbs . posMax . f


-- | groups the attribute vectors of each class
group :: Sample a -> ([[a]], InfoLabels)
group l = (gs, lbs) where
    lbs = extractLabels l
    gs = map proto (labels lbs) where proto lb = [v | (v,s)<-l , s==lb]

-- | converts a list of groups of objects into a 'Sample with labels \"1\",\"2\",...
ungroup :: [[a]] -> Sample a
ungroup gs = s where
    n = length gs
    lbs = map (show) [1 .. n]
    s = concat $ zipWith f gs lbs
    f g lb = zip g (repeat lb)

-- | Estimates the success rate of a classifier on a sample
errorRate :: Sample a -> Classifier a -> Double
errorRate exs c  = fromIntegral ok / fromIntegral (length exs) where
    ok = length [1 | (v,l)<-exs, l /= c v]

-- | Computes the confusion matrix of a classifier on a sample
confusion ::Sample a -> Classifier a -> Matrix Double
confusion exs c = confusionMatrix where
    lbs = extractLabels exs
    l = getIndex lbs
    estimatedLabels = map (l.c.fst) exs
    te = zip (map (l.snd) exs) estimatedLabels 
    nc = length (labels lbs)
    confusionMatrix = fromArray2D $ 
        accumArray (+) (0::Double) ((0::Int,0::Int),(nc-1,nc-1)) (zip te [1,1 ..])



rot 0 xs = xs
rot k (x:xs) = rot (k-1) (xs++[x])
rots l = map ((flip rot) l) [0 .. length l - 1]
auxgroup l = map (\(x:xs) -> (x, concat xs)) (rots l)

-- auxgroup x = zip x (map (concat . flip delete x) x)


-- | Constructs a (multiclass) 'Learner given any 'Dicotomizer' (by creating n features to discriminate each class against the rest)
multiclass :: Dicotomizer -> Learner (Vector Double)
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
selectClasses :: [Label] -> Sample a -> Sample a
selectClasses validset exs = filter ( (`elem` validset) .snd) exs


-- | to do
detailed machine prob = classify where
    (_,f) = machine prob
    info = snd (group prob)
    classify v = sortBy (compare `on` snd) (zip (labels info) nordsts)
        where dsts = map negate $ f v
              nordsts = map (/minimum (map abs dsts)) dsts
              on f g = \x y -> f (g x) (g y)

------------- feature combinators ------------

{- $FEATCOMB
Using them you can easily define more complex learners by composition of feature extractors (Properties). For instance:

@niceMachine = simpleMachine \`onP\` property1  \`ofP\` andP [property3, property4 `ofP` property5 ]@

Then we train the machine in a problem

@classify = niceMachine problem@

and ask for the answer: the evaluation of @classify object@ returns for example @\"apple\"@.

Composition of feature extractors is similar to ordinary function composition, but each feature is specifically created from a sample of labeled objects.

-}

-- | A function which depends on a sample
type Property a b = Sample a -> (a -> b)

-- | Applies some transformation to the objects in a Sample (it is just a map on the first element of the tuple).
preprocess :: (a -> b) -> Sample a -> Sample b
preprocess f exs = [(f v, l) | (v,l) <- exs]


-- | combines a learner with a given preprocessing stage
withPreprocess :: Property a b
               -> Learner b -> Learner a
withPreprocess method learner prob = (c,f) where
    t = method prob
    prob' = preprocess t prob
    (c',f') = learner prob'
    c = c' . t
    f = f' . t

-- | flip withPreprocess
onP :: Learner a -> Property b a -> Learner b
onP = flip withPreprocess

-- | combines several properties into a single vector (currently too restrictive)
andP :: [Property a Attributes] -> Property a Attributes
andP fs prob = f where
    ps = map ($prob) fs
    f v = join $ map ($v) ps

-- | composition of properties
ofP :: Property b c -> Property a b -> Property a c
ofP prop other prob = prop prob' . other prob where
    prob' = preprocess (other prob) prob

-- creates a property from the outputs of the estimator created by learning machine
outputOf :: Learner a -> Property a Attributes
outputOf machine prob = g where
    g v = vector $ f v
    (_,f) = machine prob

-------------------------------------

mean = meanVector . stat
cov  = covarianceMatrix . stat
meancov x = (m,c) where
    m = meanVector st
    c = covarianceMatrix st
    st = stat x

-- warning: we assume descending order in eig(R) (!?)
-- | Most discriminant linear features (lda)
mdf :: Property Attributes Attributes
mdf exs = f where
    f x = (x - m) <> v'
    n = length gs - 1
    gs = fst$ group exs
    (v',_) = fromComplex$ takeColumns n v
    (l,v) = eig (inv c <> cm)
    (m,c) = meancov$ fromRows$ map fst exs
    cm = cov$ fromRows$ map (mean.fromRows) $ gs

-- | Most expressive linear features (pca)
mef :: PCARequest -> Property Attributes Attributes
mef rq exs = encodeVector . pca rq . stat . fromRows . map fst $ exs

-----------------------------

-- | Independent attribute normalization to zero mean and variance 1
normalizeAttr :: Property Attributes Attributes
normalizeAttr exs = f where
    f x = (x-m)/d   -- FIXME: protect the division...
    xs = fromRows $ map fst exs
    m = mean xs
    d = sqrt (mean (xs*xs) - m*m)


-- | Attribute normalization, mapping the minimum and maximum to given values.
normalizeMinMax :: (Double,Double) -> Property Attributes Attributes
normalizeMinMax (mn,mx) exs = f where
    f x = b + r*x
    xs = toColumns $ fromRows $ map fst exs
    maxs = vector $ map vectorMax xs
    mins = vector $ map vectorMin xs
    dxs = maxs - mins
    dy = mx - mn
    r = dy .* recip dxs   -- FIXME: protect the division...
    b = vector [mn] - r*mins

-- | Whitening transformation
whitenAttr :: Property Attributes Attributes
whitenAttr exs = f where
    f x = w<>(x-m)
    w = whitener st
    m = meanVector st
    st = stat $ fromRows $ map fst exs

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

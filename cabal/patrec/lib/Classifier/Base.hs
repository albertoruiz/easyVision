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
-- * Probabilistic classification
     Label, Example, Sample,
      Learner, Classifier, --Predictor,
-- * Binary discriminants
     Feature, TwoGroups, Dicotomizer, -- multiclass,
-- * Utilities
     errorRate, confusion, InfoLabels(..), group, ungroup, addNoise, selectClasses, splitProportion, loadExamples, mode, -- vector, detailed,
     module Util.Stat,
-- * Feature extraction combinators
-- $FEATCOMB
     Property, withPreprocess, onP, ofP, andP, preprocess -- outputOf
) where

import Numeric.LinearAlgebra
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Array
import Util.Stat
import Util.Misc(Vec,Seed,round')
import Util.Probability(Prob,mode)


------------------- General definitions ----------------------------

type Label = String
type Example a = (a, Label)
type Sample a = [Example a]

type Classifier a = a -> Prob Label

-- | create a classifier from labeled examples
type Learner a = Sample a -> Classifier a

--type Predictor a = Classifier a -> (a -> Label)

-- | A function that tries to discriminate between two classes of objects (positive means the first class)
type Feature = Vec -> Double       -- +/-

type TwoGroups = ([Vec],[Vec])     -- +/-

-- | A learning machine for binary classification problems. See 'multiclass'.
type Dicotomizer = TwoGroups -> Feature

-- | Class labels info with helper functions
data InfoLabels = InfoLabels {
    labels :: [String],
    getIndex :: String -> Int,
    getLabel :: Int -> String
}

-----------------------------------------------------------

-- | extracts the labels of a sample
extractLabels :: Sample a -> InfoLabels
extractLabels l = InfoLabels {labels = ls, getIndex = lti, getLabel = itl}
  where
    ls = map head . L.group . L.sort . map snd $ l
    itl = (ls!!)
    m = createMap ls
    lti = (Map.!) m
    -- | given a list of labels creates a map from them to their class index
    createMap :: [String] -> Map.Map String Int
    createMap xs = Map.fromList (zip xs [0..])


-- | selects the examples with the given labels
selectClasses :: [Label] -> Sample a -> Sample a
selectClasses validset exs = filter ( (`elem` validset) .snd) exs


-- | add independent gaussian noise to the attributes
addNoise :: Seed -> Double -> Sample Vec -> Sample Vec
addNoise seed sigma l = zip rvs lbs where
    (vs,lbs) = unzip l
    n = dim (head vs)
    m = length vs
    randomVectors = toRows $ reshape n $ scalar sigma * randomVector seed Gaussian (n*m)
    rvs = zipWith (+) vs randomVectors

-- | obtains training and testing subsamples given the required proportion of training instances. You may need to scramble the original sample.
splitProportion :: Double -> [a] -> ([a],[a])
splitProportion r l = splitAt n l where n = round (r * fromIntegral (length l))

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
errorRate :: Sample a -> (a -> Label) -> Double
errorRate exs c = fromIntegral ok / fromIntegral (length exs) where
    ok = length [() | (v,l)<-exs, l /= c v]

-- | Computes the confusion matrix of a classifier on a sample
confusion ::Sample a -> (a -> Label) -> Matrix Double
confusion exs c = confusionMatrix where
    lbs = extractLabels exs
    l = getIndex lbs
    estimatedLabels = map (l.c.fst) exs
    te = zip (map (l.snd) exs) estimatedLabels
    nc = length (labels lbs)
    confusionMatrix = fromArray2D $
        accumArray (+) (0::Double) ((0::Int,0::Int),(nc-1,nc-1)) (zip te [1,1 ..])



-- | read the examples from an ASCII file.
-- Each example in a row, the last number should be an integer class code.
loadExamples :: FilePath -> IO (Sample Vec)
loadExamples filename = do
    m <- loadMatrix filename
    let c = cols m
        vs = toRows (takeColumns (c-1) m)
        ls = map (show.round') $ toList $ flatten $ dropColumns (c-1) m
    return (zip vs ls)


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
withPreprocess method learner prob = c where
    t = method prob
    prob' = preprocess t prob
    c' = learner prob'
    c = c' . t

-- | flip withPreprocess
onP :: Learner a -> Property b a -> Learner b
onP = flip withPreprocess

-- | combines several properties into a single vector (currently too restrictive)
andP :: [Property a Vec] -> Property a Vec
andP fs prob = f where
    ps = map ($prob) fs
    f v = join $ map ($v) ps

-- | composition of properties
ofP :: Property b c -> Property a b -> Property a c
ofP prop other prob = prop prob' . other prob where
    prob' = preprocess (other prob) prob

-- -- creates a property from the outputs of the estimator created by learning machine
-- outputOf :: Learner a -> Property a Vec
-- outputOf machine prob = g where
--     g v = vector $ f v
--     (_,f) = machine prob

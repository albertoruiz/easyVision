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
     Learner, Classifier, Predictor,
-- * Quality
     mode, reject, errorRate, confusion, shQuality,
-- * Binary discriminants
     Feature, TwoGroups, Dicotomizer, -- multiclass,
-- * Utilities
     group, ungroup, addNoise, selectClasses, splitProportion, loadExamples,
     InfoLabels(..),
) where

import Numeric.LinearAlgebra
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Array
import Util.Misc(Vec,Seed,round')
import Util.Probability(Prob,mode,evidence)
import Text.Printf(printf)


------------------- General definitions ----------------------------

type Label = String
type Example a = (a, Label)
type Sample a = [Example a]

type Classifier a = a -> Prob Label

-- | create a classifier from labeled examples
type Learner a = Sample a -> Classifier a

type Predictor a = a -> Maybe Label

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



-- | read the examples from an ASCII file.
-- Each example in a row, the last number should be an integer class code.
loadExamples :: FilePath -> IO (Sample Vec)
loadExamples filename = do
    m <- loadMatrix filename
    let c = cols m
        vs = toRows (takeColumns (c-1) m)
        ls = map (show.round') $ toList $ flatten $ dropColumns (c-1) m
    return (zip vs ls)


---------------------------------------------------

-- | Estimates the success rate of a classifier on a sample
errorRate :: Sample a -> (a -> Label) -> Double
errorRate exs c = fromIntegral ok / fromIntegral (length exs) where
    ok = length [() | (v,l)<-exs, l /= c v]

-- | Computes the confusion matrix of a classifier on a sample
confusion' ::Sample a -> (a -> Label) -> Matrix Double
confusion' exs c = confusionMatrix where
    lbs = extractLabels exs
    l = getIndex lbs
    estimatedLabels = map (l.c.fst) exs
    te = zip (map (l.snd) exs) estimatedLabels
    nc = length (labels lbs)
    confusionMatrix = fromArray2D $
        accumArray (+) (0::Double) ((0::Int,0::Int),(nc-1,nc-1)) (zip te [1,1 ..])


-- | Computes the confusion matrix taking into account rejection (last column).
confusion ::Sample a -> (a -> Maybe Label) -> Matrix Double
confusion exs c = confusionMatrix where
    (_,lbs) = group exs
    l = getIndex lbs
    estimatedLabels = map (r l.c.fst) exs
    te = zip (map (l.snd) exs) estimatedLabels
    nc = length (labels lbs)
    confusionMatrix = fromArray2D $
        accumArray (+) (0::Double) ((0::Int,0::Int),(nc-1,nc)) (zip te [1,1 ..])
    r _ Nothing = nc
    r f (Just x)  = f x


-- | If the evidence is less than the desired decibels we return Nothing
reject :: Double -> Prob String -> Maybe String
reject db p = if evidence m p < db
                then Nothing
                else Just m
    where m = mode p

-- | Display the confusion matrix, error and rejection rates.
shQuality :: Sample a -> (a -> Maybe Label) -> IO ()
shQuality d c = do
    let m = confusion d c
        ac = sumElements (takeDiag m)
        rj = sumElements (dropColumns (rows m) m)
        tt = sumElements m
    print (tt,ac,rj)
    _ <- printf "Rej: %.2f %%  Err: %.2f\n" (100 * rj / tt) (100 - 100 * ac / (tt-rj))
    putStrLn $ format " " (show.round') m

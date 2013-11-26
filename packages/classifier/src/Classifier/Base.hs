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
     mode, reject, CQuality(..), quality, shQuality,
-- * Binary discriminants
     Feature, TwoGroups, Dicotomizer,
-- * Utilities
     group, ungroup, vectorLabels, softMax, loglik2prob,
     addNoise, selectClasses, splitProportion, loadExamples,
     InfoLabels(..),
) where

import Numeric.LinearAlgebra
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Array
import Util.Misc(vec,Vec,Mat,Seed)
import Util.Probability(Prob,mode,evidence,weighted)
import Text.Printf(printf)


------------------- General definitions ----------------------------

type Label = String
type Example a = (a, Label)
type Sample a = [Example a]

type Classifier a = a -> Prob Label

-- | create a classifier from labeled examples
type Learner a = Sample a -> Classifier a

-- | decision function with possible rejection
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

-- | replace string classes by positional codification
vectorLabels :: Sample a -> [(a, Vec)]
vectorLabels s = x where
    lbs = snd (group s)
    nc = length (labels lbs)
    des k c = vec $ replicate (k-1) (-1) ++ [1] ++ replicate (c-k) (-1)
    x = [(v, des (1+getIndex lbs l) nc) | (v,l) <- s]

softMax :: Vec -> Vec
softMax v = u / scalar (sumElements u)
  where u = exp v

-- | softMax with label info
loglik2prob :: InfoLabels -> [Double] -> Prob Label
loglik2prob lbs fs = weighted $ zip (labels lbs) (map exp fs)

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

-- | Computes the confusion matrix taking into account rejection (last column).
confusion' ::Sample a -> Predictor a -> Matrix Double
confusion' exs c = confusionMatrix where
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

data CQuality = CQuality { confusion :: Mat
                         , errorRate :: Double
                         , rejectionRate :: Double }

-- | confusion matrix, error and rejection rates.
quality :: Sample a -> Predictor a -> CQuality
quality d c = CQuality { confusion = m
                       , rejectionRate = 100 * rj / tt
                       , errorRate = 100 - 100 * ac / (tt-rj) }
  where
    m = confusion' d c
    (ac,rj,tt) = art m

art :: Mat -> (Double,Double,Double)
art m = (ac,rj,tt)
  where
    ac = sumElements (takeDiag m)
    rj = sumElements (dropColumns (rows m) m)
    tt = sumElements m

shQuality :: CQuality -> IO ()
shQuality q = do
    let m = confusion q
        (ac,rj,tt) = art m
    _ <- printf "%d/%d/%d\n" (round' ac) (round' rj) (round' tt)
    _ <- printf "Rej: %.2f %%  Err: %.2f %%\n" (rejectionRate q) (errorRate q)
    putStrLn $ format " " (show.round') m

round' :: RealFrac a => a -> Int
round' x = round x :: Int


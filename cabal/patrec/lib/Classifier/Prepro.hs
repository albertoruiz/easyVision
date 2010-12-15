-----------------------------------------------------------------------------
{- |
Module      :  Classifier.Prepro
Copyright   :  (c) Alberto Ruiz 2006-10
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  portable

Preprocessing methods and combinators.

Using them you can easily define more complex learners by composition of feature extractors (Properties). For instance:

@niceMachine = simpleMachine \`onP\` property1  \`ofP\` andP [property3, property4 `ofP` property5 ]@

Then we train the machine in a problem

@classify = niceMachine problem@

and ask for the answer: the evaluation of @classify object@ returns for example @\"apple\"@.

Composition of feature extractors is similar to ordinary function composition, but each feature is specifically created from a sample of labeled objects.


-}
-----------------------------------------------------------------------------

module Classifier.Prepro (
-- * Feature extraction combinators
     Property, withPreprocess, onP, ofP, andP, preprocess, -- outputOf
-- * Linear feature extraction
     mef, mdf,
-- * Attribute normalization
     whitenAttr, normalizeAttr, normalizeMinMax, boxAttr,
) where

import Util.Stat
import Numeric.LinearAlgebra
import Classifier.Base
import Util.Misc(Vec,vec,Mat)


------------- feature combinators ------------

{- $FEATCOMB

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

-----------------------------------------------------------------

mean :: Mat -> Vec
mean = meanVector . stat

cov :: Mat -> Mat
cov  = covarianceMatrix . stat

-- warning: we assume descending order in eig(R) (!?)
-- | Most discriminant linear features (lda)
mdf :: Property Vec Vec
mdf exs = f where
    f x = (x - m) <> v'
    n = length gs - 1
    gs = fst$ group exs
    (v',_) = fromComplex $ takeColumns n v
    (_l,v) = eig (inv c <> cm)
    (m,c) = meanCov $ fromRows $ map fst exs
    cm = cov $ fromRows $ map (mean.fromRows) $ gs

-- | Most expressive linear features (pca)
mef :: PCARequest -> Property Vec Vec
mef req exs = encodeVector . pca req . stat . fromRows . map fst $ exs

-----------------------------

-- | Independent attribute normalization to zero mean and variance 1
normalizeAttr :: Property Vec Vec
normalizeAttr exs = f where
    f x = (x-m)/d   -- FIXME: protect the division...
    xs = fromRows $ map fst exs
    m = mean xs
    d = sqrt (mean (xs*xs) - m*m)


-- | Attribute normalization, mapping the minimum and maximum to given values.
normalizeMinMax :: (Double,Double) -> Property Vec Vec
normalizeMinMax (mn,mx) exs = f where
    f x = b + r*x
    xs = toColumns $ fromRows $ map fst exs
    maxs = vec $ map maxElement xs
    mins = vec $ map minElement xs
    dxs = maxs - mins
    dy = mx - mn
    r = dy `scale` recip dxs   -- FIXME: protect the division...
    b = vec [mn] - r*mins

-- | Whitening transformation
whitenAttr :: Property Vec Vec
whitenAttr exs = f where
    f x = w<>(x-m)
    w = whitener st
    m = meanVector st
    st = stat $ fromRows $ map fst exs

-- | Centering on the mean and isotropic scaling to max = 4
boxAttr :: Property Vec Vec
boxAttr exs = f where
    m = fromRows (map fst exs)
    med = meanVector (stat m)
    mx = maxElement (abs (flatten $ m - asRow med))
    f x = scalar (4/mx) * (x-med)


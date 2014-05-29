-----------------------------------------------------------------------------
{- |
Module      :  Classifier.Adaboost
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

The adaboost metalearner.

-}
-----------------------------------------------------------------------------

module Classifier.Adaboost (
-- * Working with weighted examples
    Weights, WeightedDicotomizer, unweight, weight,
-- * stumps
-- | an extremely simple learner
     stumps, singlestump,
-- * other simple learners with weighted examples
     mseWeighted, distWeighted,
-- * Meta algorithms
-- ** Adaboost
     adaboost, adaboostST, adaFun
) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util(norm,(&),(¦),(#))
import Classifier.Base
import Classifier.Simple(multiclass)
import Data.List(sortBy, transpose,partition)
import Util.Misc(posMin)
import Data.Function(on)
import Util.Misc(Vec,vec,posMax)
import System.Random

type Weights = Vector Double
-- | An improved 'Dicotomizer' which admits a distribution on the given examples.
type WeightedDicotomizer = TwoGroups -> Weights -> Feature


-- | an extremely simple learning machine
singlestump :: Learner Vec
singlestump = multiclass (unweight stumps)


-- | weak learner which finds a good threshold on a single attribute
stumps :: WeightedDicotomizer
stumps p = stumpsOk (prepro p)

-- useful precomputation: indices of the sorted features.
prepro :: TwoGroups -> (([Vec], [Vec]), Vec, [[Double]], [[Double]], [[Int]])
prepro (g1,g2) = ((g1,g2),lbs,xs,oxs,is) where
    lbs = vjoin [constant 1 (length g1), constant (-1) (length g2)]
    xs = transpose $ map toList (g1 ++ g2)
    s = map f xs
    f l = sortBy (compare `on` fst) (zip l [0..])
    oxs = map (map fst) s
    is  = map (map snd) s

stumpsOk :: (([a], [b]), Vec, t, [[Double]], [[Int]]) -> Vec -> Feature
stumpsOk ((g1,g2),lbs,_xs,oxs,is) d = f where
    wl = lbs*d
    n1 = length g1
    n2 = length g2
    d1 = pnorm PNorm1 $ subVector  0 n1 d
    d2 = pnorm PNorm1 $ subVector n1 n2 d

    owls = map (map (wl@>)) is
    cs = map (sel .dt . scanl (+) 0 . init) owls
    dt x = ((k,v),(q,w)) where
        k = posMin x
        v = (x!!k) + d2
        q = posMax x
        w = d1 - (x!!q)
    sel ((k,v),(q,w)) = if v < w then (k,v,1) else (q,w,-1)
    r = map g $ zip oxs cs
    g (l,(k,v,s)) = (v,(h l k, s))
    h l k = 0.5*(l'!!(k) + l'!!(k+1)) where
        l' = (l!!0 - (l!!1-l!!0)) : l -- ++ [l!!n + (l!!n - l!!(n-1))]
        -- n = length l - 1
    j = {- debug $ -} posMin (map (abs.fst) r)
    (_,(u,sg)) = r!!j

    f x = sg * signum' (x @> j - u)
    signum' x = if x > 0 then 1 else -1


-- | weak learner trained by the adaboost algorithm
type ADBST = (Feature, Vec, Double, Double)


-- this works with a partially applied WeightedDicotomizer to reuse any
-- possible data preprocessing
adaboostStep :: (Weights -> Feature) -> TwoGroups -> Weights -> ADBST
adaboostStep method (g1,g2) d = (f,d',e,a) where
    f = method d
    e1 = map (signum . max 0 . negate . f) g1
    e2 = map (signum . max 0 . f) g2
    e = vjoin [vec e1, vec e2] <.> d
    a = 0.5 * log ((1-e)/e) -- it may be Infinity
    kp = exp (-a)
    kn = exp a
    f1 v = if f v > 0 then kp else kn
    f2 v = if f v < 0 then kp else kn
    d1 = map f1 g1
    d2 = map f2 g2
    dr = d * vjoin [vec d1, vec d2]
    d' = dr / scalar (dr <.> constant 1 (dim dr))

-- | creates a list of weak learners and associated information to build a strong learner using adaboost
adaboostST :: Int -> WeightedDicotomizer -> TwoGroups -> [ADBST]
adaboostST n m p = r where
    (f,st@(g,_d,e,_a)) = initAdaboost m p
    work = takeok n (iterate (adaboost' f p) st)
    easy = [(g,1,e,1)]
    r = if e > 0.001 then work else easy
    adaboost' h q (_,d,_,_) = adaboostStep h q d
    takeok n' = take n' . fst . span pos
    pos (_,_,err,_) = err < 0.499


initAdaboost :: WeightedDicotomizer -> TwoGroups -> (Weights -> Feature, ADBST)
initAdaboost method gs =  (f, adaboostStep f gs w)
    where f = method gs
          w = constant (1 / fromIntegral m) m
          m = length g1 + length g2 where (g1,g2) = gs

-- | combines the weak learners obtained by 'adaboostST'
adaFun :: [ADBST] -> Feature
adaFun st = comb where
    comb x = sum $ [a*signum (f x)| (f,_,_,a) <- st]

-- runs directly n steps of the adaboost algorithm
adaboost:: Int -> WeightedDicotomizer -> Dicotomizer
adaboost n m = adaFun . adaboostST n m


----------------------------------------------------------

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


------------------------------------------------------------------------------------

-- more complex weak learners, rather bad

-- | mse with weighted examples
mseWeighted :: WeightedDicotomizer
mseWeighted (g1,g2) d = f where
    m = (fromRows g1 # fromRows g2) ¦ konst 1 (dim b,1)
    b = constant 1 (length g1) & constant (-1) (length g2)
    rd  = sqrt d
    rd' = outer rd (constant 1 (cols m))
    w = (m*rd') <\> (b*rd)
    f v = tanh ((v & 1) <.> w)


-- | a minimum distance dicotomizer using weighted examples
distWeighted :: WeightedDicotomizer
distWeighted (g1,g2) d = f where
    n1 = length g1
    n2 = length g2
    d1 = subVector  0 n1 d
    d2 = subVector n1 n2 d
    ones = constant 1 (dim (head g1))
    a1 = outer d1 ones * fromRows g1
    a2 = outer d2 ones * fromRows g2
    m1 = sumColumns a1 / scalar (pnorm PNorm1 d1)
    m2 = sumColumns a2 / scalar (pnorm PNorm1 d2)
    f x = norm (x-m2) - norm (x-m1)
    sumColumns m = constant 1 (rows m) <> m

-- just to check that they are not completely wrong

--mse' = multiclass (unweight mseWeighted)

--dist' = multiclass (unweight distWeighted)


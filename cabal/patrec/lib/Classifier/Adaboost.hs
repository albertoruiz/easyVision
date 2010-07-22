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
-- * stumps
-- | an extremely simple learner
     stumps,
-- * Meta algorithms
-- ** Adaboost
     adaboost, adaboostST, adaFun,
-- ** Decision tree
     treeOf, branch

) where

import Numeric.LinearAlgebra
import Classifier.Base
import Data.List(sortBy, transpose,partition)
import Util.Misc(posMin)
import Data.Function(on)

-- -- | an extremely simple learning machine
-- singlestump :: Learner (Vector Double)
-- singlestump = multiclass (unweight stumps)


-- | weak learner which finds a good threshold on a single attribute
stumps :: WeightedDicotomizer
stumps p = stumpsOk (prepro p)

-- useful precomputation: indices of the sorted features.
prepro :: (Ord a, Num b, Enum b, Element t, Element a)
       => ([Vector a], [Vector a]) -> (([Vector a], [Vector a]), Vector t, [[a]], [[a]], [[b]])
prepro (g1,g2) = ((g1,g2),lbs,xs,oxs,is) where
    lbs = join [constant 1 (length g1), constant (-1) (length g2)]
    xs = transpose $ map toList (g1 ++ g2)
    s = map f xs
    f l = sortBy (compare `on` fst) (zip l [0..])
    oxs = map (map fst) s
    is  = map (map snd) s


stumpsOk :: (Ord a2, Num t1, Element a2)
         => (([a1], [a]), Vector Double, t, [[a2]], [[Int]])
         -> Vector Double -> Vector a2 -> t1
stumpsOk ((g1,g2),lbs,_xs,oxs,is) d = f where
    wl = lbs*d
    n1 = length g1
    n2 = length g2
    d1 = pnorm PNorm1 $ subVector  0 n1 d
    d2 = pnorm PNorm1 $ subVector n1 n2 d

    owls = map (map (wl@>)) is
    cs = map (sel .dt . scanl (+) 0 . init) owls
    dt x = ((k',v'),(q,w)) where
        k' = posMin x
        v' = (x!!k) + d2
        q = posMax x
        w = d1 - (x!!q)
    sel ((k',v'),(q,w)) = if v' < w then (k',v',1) else (q,w,-1)
    r = map g $ zip oxs cs
    g (l,(k',v',s')) = (v',(h l k', s'))
    h l k' = 0.5*(l'!!k' + l'!!(k'+1)) where
        l' = (l!!0 - (l!!1-l!!0)) : l -- ++ [l!!n + (l!!n - l!!(n-1))]
        -- n = length l - 1
    k = {- debug $ -} posMin (map (abs.fst) r)
    (_,(v,s)) = r!!k

    f x = s * signum' (x @> k - v)
    signum' x = if x > 0 then 1 else -1


-- | weak learner trained by the adaboost algorithm
type ADBST = (Feature, Vector Double, Double, Double)


-- this works with a partially applied WeightedDicotomizer to reuse any
-- possible data preprocessing
adaboostStep :: (Weights -> Feature) -> TwoGroups -> Weights -> ADBST
adaboostStep method (g1,g2) d = (f,d',e,a) where
    f = method d
    e1 = map (signum . max 0 . negate . f) g1
    e2 = map (signum . max 0 . f) g2
    e = join [vector e1, vector e2] <.> d
    a = 0.5 * log ((1-e)/e) -- it may be Infinity
    kp = exp (-a)
    kn = exp a
    f1 v = if f v > 0 then kp else kn
    f2 v = if f v < 0 then kp else kn
    d1 = map f1 g1
    d2 = map f2 g2
    dr = d * join [vector d1, vector d2]
    d' = dr / scalar (dr <.> constant 1 (dim dr))

-- | creates a list of weak learners and associated information to build a strong learner using adaboost
adaboostST :: Int -> WeightedDicotomizer -> TwoGroups -> [ADBST]
adaboostST n m p = r where
    (f,st@(g,_d,e,_a)) = initAdaboost m p
    work = takeok n (iterate (adaboost' f p) st)
    easy = [(g,1,e,1)]
    r = if e > 0.001 then work else easy
    adaboost' fun q (_,d,_,_) = adaboostStep fun q d
    takeok k = take k . fst . span pos
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

------------------------------------------------------------------------

-- | Creates a decision tree
treeOf :: (TwoGroups -> Bool) -> Dicotomizer -> Dicotomizer

treeOf stopQ method gs@(g1,g2) = if stopQ gs || not improved then leaf else node where
    n1 = length g1
    n2 = length g2
    leaf = if n1>n2 then const 1 else const (-1)
    node v = if d v > 0 then d1 v else d2 v
    d = method gs
    (g11,g12) = partition ((>0).d) g1
    (g21,g22) = partition ((>0).d) g2
    d1 = treeOf stopQ method (g11,g21)
    d2 = treeOf stopQ method (g12,g22)
    improved = (length g11, length g21) /= (n1,n2) &&
               (length g12, length g22) /= (n1,n2)

-- | stopping criterium for 'treeOf'. A new decision node is created if the minoritary class has more than n samples
branch :: Int -> (TwoGroups -> Bool)
branch n (g1,g2) = min (length g1) (length g2) <= n

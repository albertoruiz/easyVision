-----------------------------------------------------------------------------
{- |
Module      :  Util.Probability
Copyright   :  (c) Alberto Ruiz 2010
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  portable

A stripped-down version of the probability monad.

See the works by Erwig and Kollmansberger, Eric Kidd, Dan Piponi, etc.

-}
-----------------------------------------------------------------------------

module Util.Probability (
    -- * Probability Rules
    Prob,
    joint,
    marg,
    cond,
    compose,
    -- * Tools
    evidence,
    mode,
    prob,
    unprob,
    -- * Simple distributions
    weighted, uniform, bernoulli,
    -- * Misc
    collect,
    jointWith,
    pfilter, pmap
) where

import Data.Function(on)
import Data.List(sortBy,groupBy,intercalate)
import Util.Misc(posMax)
import Text.Printf(printf)
import Control.Monad(ap)
import Control.Applicative

data Prob a = Prob [(a,Double)] deriving Eq

mapv :: (a -> b) -> [(a, p)] -> [(b, p)]
mapv f = map $ \(v,p) -> (f v, p)

mapp :: (a -> b) -> [(v, a)] -> [(v, b)]
mapp f = map $ \(v,p) -> (v, f p)

sump :: Num b => [(a, b)] -> b
sump = sum . map snd

collect :: Ord x => Prob x -> Prob x
collect (Prob xs)
    = Prob
    . norm
    . map add
    . groupBy ((==) `on` fst)
    . sortBy (compare `on` fst)
    $ xs
  where
    add rs = ((fst.head) rs, sump rs)
    norm ys = mapp (/t) ys where t = sump ys

instance Functor Prob where
    fmap f (Prob t) = Prob $ mapv f t

instance Monad Prob where
    return x = Prob [(x,1)]
    t >>= f = pjoin (fmap f t)

pjoin :: Prob (Prob a) -> Prob a
pjoin (Prob xs) = Prob $ concat [ mapp (*p) y | (Prob y, p) <- xs]

instance Applicative Prob where
   (<*>) = ap
   pure = return

---------------------------------------------------------

instance (Ord a, Show a) => Show (Prob a) where
    show y = intercalate "\n" (map each xs)
      where
        Prob xs = collect y
        each (x,p) = printf "%6.2f %% %s" (100*p) (show x)


---------------------------------------------------------

-- | joint probability with a general grouping function
jointWith :: (Ord x) => (t -> a -> x) -> (a -> Prob t) -> Prob a -> Prob x
jointWith p' g' f' = collect $ f' >>= h g' p'
  where
    h g p = \a -> do
        b <- g a
        return (p b a)

-- | joint probability using a tuple
joint :: (Ord a, Ord b) => (a -> Prob b) -> Prob a -> Prob (b, a)
joint g f = jointWith ((,)) g f

---------------------------------------------------------

pfilter :: (Ord a) => (a -> Bool) -> Prob a -> Prob a
pfilter pre (Prob xs) = collect . Prob . filter (pre.fst) $ xs

pmap :: (Ord x) => (a -> x) -> Prob a -> Prob x
pmap fun = collect . fmap fun

cond :: (Ord a) => Prob a -> (a -> Bool) -> Prob a
cond = flip pfilter

marg :: (Ord x) => Prob a -> (a -> x) -> Prob x
marg = flip pmap

compose :: (Ord c) => (a -> Prob b) -> (b -> Prob c) -> (a -> Prob c)
compose f g = h where
    h x = collect ( f x >>= g )

---------------------------------------------------------

prob :: (Eq a) => a -> Prob a -> Double
prob a (Prob xs) = maybe 0 id (lookup a xs)

--------------------------------------------------------

unprob :: Ord t => Prob t -> [(t, Double)]
unprob = f . collect
  where
    f (Prob xs) = xs

---------------------------------------------------------

-- | in db
evidence :: (Ord a) => a -> Prob a -> Double
evidence s x = 10 * logBase 10 (a/b)
    where Prob y = collect x
          a = sum $ map snd $ filter ((s==).fst) y
          b = 1 - a

mode :: (Ord a) => Prob a -> a
mode ys = fst (xs!!k)
    where Prob xs = collect ys
          k = posMax (map snd xs)

----------------------------------------------------------

-- | create a probability distribution from relative weights
weighted :: (Ord x) => [(x, Double)] -> Prob x
weighted xs = collect (Prob xs)

-- | if there are repeated elements the result is not uniform...
uniform :: (Ord a) => [a] -> Prob a
uniform xs = weighted (map (\x->(x,1)) xs)

bernoulli :: (Ord x) => Double -> x -> x -> Prob x
bernoulli p a b = weighted [(a,p),(b,1-p)]

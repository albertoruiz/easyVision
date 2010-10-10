-----------------------------------------------------------------------------
{- |
Module      :  Classifier.Tree
Copyright   :  (c) Alberto Ruiz 20010
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional

A simple classification tree

-}
-----------------------------------------------------------------------------


module Classifier.Tree (
     treeOf, branch

) where

import Numeric.LinearAlgebra
import Classifier.Base

import Data.List(sortBy, sort, nub, elemIndex, intersperse, transpose, partition, delete)
import qualified Data.Map as Map
import System.Random
import Data.Array hiding((//))
import Util.Stat
import Util.Misc(norm,(&),(//),(#))
import qualified Data.Map as M
import Data.Maybe(fromMaybe)
import qualified Data.List as L
import Control.Arrow((&&&))

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

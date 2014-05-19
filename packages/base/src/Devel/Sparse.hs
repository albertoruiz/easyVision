{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}



module Devel.Sparse(
    SVector(..),
    SMatrix(..),
    toDense,
    toAssocs,
    fromAssocs,
    strans
)where

import Numeric.Container
import qualified Data.Vector.Storable as V
import Data.Function(on)
import Control.Arrow((***))
import Data.List(groupBy, sortBy)

data SVector t = SVector
    { pos :: Vector Int
    , val :: Vector t
    } deriving Show

data SMatrix t = SMatrix { srows :: [SVector t] }

sdot :: Product t => SVector t -> Vector t -> t
sdot (SVector sp sv) u = V.backpermute u sp `udot` sv

smXv :: Product t => SMatrix t -> Vector t -> Vector t
smXv (SMatrix m) v = fromList $ map (flip sdot v) m


instance Product t => Contraction (SVector t) (Vector t) t
  where
    (<.>) = sdot

instance Product t => Contraction (SMatrix t) (Vector t) (Vector t)
  where
    (<.>) = smXv


toDense :: Container Vector t => SMatrix t -> Matrix t
toDense m = assoc (r+1,c+1) 0 l
  where
    l = toAssocs m
    (r,c) = (maximum *** maximum) $ unzip $ map fst l


toAssocs :: Element t => SMatrix t -> [((Int,Int),t)]
toAssocs = concat . zipWith f [(0::Int)..] . srows
  where
    f i (SVector sp sv) = zipWith g (toList sp) (toList sv)
      where
        g j v = ((i,j),v)


fromAssocs :: Element t => [((Int, Int), t)] -> SMatrix t
fromAssocs = SMatrix . map (uncurry SVector . (fromList *** fromList) . unzip . map (snd *** id)) . groupBy ((==) `on` (fst.fst)) . sortBy (compare `on` fst)


strans :: Element t => SMatrix t -> SMatrix t
strans = fromAssocs . map (\((i,j),v)->((j,i),v)) . toAssocs


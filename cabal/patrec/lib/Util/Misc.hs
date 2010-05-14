-- | General purpose functions

module Util.Misc where

import Numeric.LinearAlgebra
import Debug.Trace
import Data.Function(on)
import Data.List(elemIndex, sortBy, sort)
import System.Random
import qualified Data.Array as A

splitEvery :: Int -> [t] -> [[t]]
splitEvery _ [] = []
splitEvery k l = take k l : splitEvery k (drop k l)

pairsWith :: (b -> b -> a) -> [b] -> [a]
pairsWith _ [] = []
pairsWith _ [_] = []
pairsWith f (x:xs) = map (f x) xs ++ pairsWith f xs

debug :: (Show a) => String -> (a1 -> a) -> a1 -> a1
debug msg f x = trace (msg ++ show (f x)) x

-- | returns the position of the maximum element.
posMax :: Ord a => [a] -> Int
posMax l = p where
    Just p = elemIndex (maximum l) l

-- | returns the position of the maximum element.
posMin :: Ord a => [a] -> Int
posMin l = p where
    Just p = elemIndex (minimum l) l

-- | pseudorandom permutation of a list
randomPermutation :: Int -> [a] -> [a]
randomPermutation seed l = map fst $ sortBy (compare `on` snd) randomTuples where
    randomTuples = zip l (randomRs (0, 1::Double) (mkStdGen seed))

-- Euclidean vector norm.
norm :: Vector Double -> Double
norm x = pnorm PNorm2 x

mean :: (Fractional a) => [a] -> a
mean l = sum l / fromIntegral (length l)

median :: (Ord a) => [a] -> a
median l = sort l !! (div (length l) 2)

arrayOf :: [a] -> (Int -> a)
arrayOf xs = (A.listArray (0, length xs -1) xs A.!)

myintersect :: (Ord a) => [a] -> [a] -> [a]
myintersect as bs = go as bs [] where
    go [] _ x = x
    go _ [] x = x
    go (a:as) (b:bs) x
        | a < b = go (a:as) bs x
        | a > b = go as (b:bs) x
        | otherwise = go as bs (a:x)

intersectSorted :: (Ord a) => [a] -> [a] -> [a]
intersectSorted as bs = reverse (go as bs []) where
    go [] _ x = x
    go _ [] x = x
    go (a:as) (b:bs) x
        | a > b = go (a:as) bs x
        | a < b = go as (b:bs) x
        | otherwise = go as bs (a:x)


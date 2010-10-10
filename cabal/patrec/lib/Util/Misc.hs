-- | General purpose functions

module Util.Misc where

import Numeric.LinearAlgebra
import Debug.Trace
import Data.Function(on)
import Data.List(elemIndex, sortBy, sort)
import System.Random
import qualified Data.Array as A

type Mat = Matrix Double
type Vec = Vector Double

vec :: [Double] -> Vec
vec = fromList

type Seed = Int

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

-- | Euclidean vector norm.
norm :: Vector Double -> Double
norm x = pnorm PNorm2 x

mean :: (Fractional a) => [a] -> a
mean l = sum l / fromIntegral (length l)

median :: (Ord a) => [a] -> a
median l = sort l !! (div (length l) 2)

quartiles :: (Ord a) =>  [a] -> (a,a,a)
quartiles l = (f a, f b, f c) where
    f = (s!!)
    s = sort l
    n = fromIntegral (length l)
    [a,b,c] = map (round' . (*(n-1))) [0.25,0.5,0.75::Double]

arrayOf :: [a] -> (Int -> a)
arrayOf xs = (A.listArray (0, length xs -1) xs A.!)

myintersect :: (Ord a) => [a] -> [a] -> [a]
myintersect xs ys = go xs ys [] where
    go [] _ x = x
    go _ [] x = x
    go (a:as) (b:bs) x
        | a < b = go (a:as) bs x
        | a > b = go as (b:bs) x
        | otherwise = go as bs (a:x)

intersectSorted :: (Ord a) => [a] -> [a] -> [a]
intersectSorted xs ys = reverse (go xs ys []) where
    go [] _ x = x
    go _ [] x = x
    go (a:as) (b:bs) x
        | a > b = go (a:as) bs x
        | a < b = go as (b:bs) x
        | otherwise = go as bs (a:x)


-- | replace elements of xs at given positions by ys
replaceAt :: [Int] -> [a] -> [a] -> [a]
replaceAt pos ys xs = zipWith f [0..] xs where
    g = flip lookup (zip pos ys)
    f k x = case g k of
        Just y -> y
        Nothing -> x

-- | used to avoid incomplete patterns
impossible :: String -> a
impossible msg = error ("impossible input in "++ msg)

-- | specialized @(^2)@
sqr :: Num a => a -> a
sqr x = x^(2::Int)

-- | specialized 'round'
round' :: RealFrac a => a -> Int
round' = round

-- | Concatenation of real vectors.
infixl 9 #
(#) :: Vec -> Vec -> Vec
a # b = join [a,b]

-- | Horizontal concatenation of real matrices.
infixl 8 &
(&) :: Mat -> Mat -> Mat
a & b = fromBlocks [[a,b]]

-- | Vertical concatenation of real matrices.
infixl 7 //
(//) :: Mat -> Mat-> Mat
a // b = fromBlocks [[a],[b]]

{-# LANGUAGE FlexibleContexts #-}

-- | General purpose functions

module Util.Misc(
    -- * List utilities
    splitEvery, pairsWith, posMax, posMin, replaceAt, rep, selectPos, rotateLeft, sliding, slidingPeriodic, subListsBy, takeUntil,
    intersectSorted, unionSort,
    -- * Useful constants
    degree, stdpix, Seed,
    -- * Linear Algebra
    Mat, mat, Vec, vec,
    unliftVector, unliftRow,
    norMax, nulln, orthm,
    -- * Misc
    angleDiff,
    lambdify, (.:), (//),
    arrayOf, memo,
    Cyclic(..), mkCyclic, cycAt
) where


import Numeric.LinearAlgebra.HMatrix hiding ((!))
import Data.List(elemIndex, sort, group, isPrefixOf, tails)
import qualified Data.Array as A
import Foreign.Storable(Storable)
import Data.Array(listArray,(!))
import qualified Data.Vector as V
import Data.Packed.Development((//))


type Mat = Matrix Double
type Vec = Vector Double

vec :: [Double] -> Vec
vec = fromList

mat :: [[Double]] -> Mat
mat = fromLists



splitEvery :: Int -> [t] -> [[t]]
splitEvery _ [] = []
splitEvery k l = take k l : splitEvery k (drop k l)

pairsWith :: (b -> b -> a) -> [b] -> [a]
pairsWith _ [] = []
pairsWith _ [_] = []
pairsWith f (x:xs) = map (f x) xs ++ pairsWith f xs

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil q xs = a++ take 1 b
  where
    (a,b) = break q xs


-- | returns the position of the maximum element.
posMax :: Ord a => [a] -> Int
posMax l = p where
    Just p = elemIndex (maximum l) l

-- | returns the position of the minimum element.
posMin :: Ord a => [a] -> Int
posMin l = p where
    Just p = elemIndex (minimum l) l



arrayOf :: [a] -> (Int -> a)
arrayOf xs = (A.listArray (0, length xs -1) xs A.!)

-- | memoize a function for arguments in [-a,a]
memo :: Int -> (Int -> a) -> (Int -> a)
memo mx f = g where
    m = listArray (-mx,mx::Int) [f k | k <- [-mx..mx]]
    g w = m ! w

--------------------------------------------------------------------------------

-- | O(1) access to a periodic array
newtype Cyclic a = Cyclic { cycVect :: V.Vector a }

mkCyclic :: [a] -> Cyclic a
mkCyclic = Cyclic . V.fromList

cycAt :: Cyclic a -> Int -> a
cycAt (Cyclic v) k = v V.! (mod k (V.length v))

--------------------------------------------------------------------------------

-- | intersection of two lists of ordered elements (the result is also ordered).
intersectSorted :: (Ord a) => [a] -> [a] -> [a]
intersectSorted xs ys = reverse (go xs ys []) where
    go [] _ x = x
    go _ [] x = x
    go (a:as) (b:bs) x
        | a > b = go (a:as) bs x
        | a < b = go as (b:bs) x
        | otherwise = go as bs (a:x)

-- | union for elements in Ord (the lists need not be ordered, but the result will be)
unionSort :: (Ord a) => [[a]] -> [a]
unionSort = map head . group . sort . concat


-- | replace elements of xs at given positions by ys
replaceAt :: [Int] -> [a] -> [a] -> [a]
replaceAt pos ys xs = zipWith f [0..] xs where
    g = flip lookup (zip pos ys)
    f k x = case g k of
        Just y -> y
        Nothing -> x

-- | replace substring
rep :: Eq a => ([a], [a]) -> [a] -> [a]
rep (_,_) [] = []
rep (c,r) f@(x:xs)
  | c `isPrefixOf` f = r ++ rep (c,r) (drop (length c) f)
  | otherwise        = x:(rep (c,r) xs)


-- | select elements in a list at given positions
selectPos :: (Num a, Enum a, Eq a) => [a] -> [b] -> [b]
selectPos is = map snd . filter (flip elem is . fst) . zip [0 ..]


-- | conversion factor from degrees to radians
degree :: Double
degree = pi/180


stdpix :: Double
-- ^ size of one pixel in a 640x480 image, in normalized image coordinates
stdpix = 2/640


unliftVector :: Storable a => (Vector a -> Vector a) -> ([a] -> [a])
unliftVector f = toList . f . fromList

unliftRow :: Element a => (Matrix a -> Matrix a) -> (Vector a -> Vector a)
unliftRow f = flatten . f . asRow



-- | Taken from \"not-in-base\" package.
lambdify :: (a -> b -> t) -> (c -> a) -> (c -> b) -> c -> t
lambdify f a b x = f (a x) (b x)

-- | composition for 1 and 2 argument functions

infixr 9 .:
(.:) :: (x -> y) -> ( a -> b-> x) -> (a -> b-> y)
f .: g = \a b -> f (g a b)

--------------------------------------------------------------------

rotateLeft :: Int -> [a] -> [a]
rotateLeft n list | n >= 0    =  take len $ drop n $ cycle list
                  | otherwise =  rotateLeft (len + n) list
  where
    len = length list

sliding :: Int -> [a] -> [[a]]
sliding k = filter ((==k).length) . map (take k) . tails

slidingPeriodic :: Int -> [a] -> [[a]]
slidingPeriodic k xs = take n . sliding k . cycle $ xs
  where
    n = length xs

----------------------------------------------------------------------

subListsBy :: (a -> Bool) -> [a] -> [[a]]
-- ^ split a list into parts of consecutive elements satisfying a predicate
subListsBy _ [] = []
subListsBy p xs = case ok of
    [] -> subListsBy p (snd $ span (not.p) xs)
    _  -> ok : subListsBy p rest
  where
    (ok,rest) = span p xs

----------------------------------------------------------------------

-- FIXME
angleDiff :: (Floating a, Ord a) => a -> a -> a
-- ^ absolute difference between two angles
angleDiff x y = min (abs (x-y)) (abs (opos x - opos y))
  where
    opos z = if z > 0 then z - pi else z + pi

----------------------------------------------------------------------

norMax :: (Num (c e), Container c e) => e -> c e -> c e
norMax s m = m * scalar (s / v)
  where
    p = maxIndex (abs m)
    v = m `atIndex` p

--------------------------------------------------------------------------------

nulln :: Int -> Matrix Double -> [Vector Double]
nulln n = reverse . take n . reverse . toColumns . snd . rightSV

orthm :: Matrix Double -> Matrix Double
orthm = orth

--------------------------------------------------------------------------------


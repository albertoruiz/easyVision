-- | General purpose functions

module Util.Misc where

import Numeric.LinearAlgebra
import Debug.Trace
import Data.Function(on)
import Data.List(elemIndex, sortBy, sort, group)
import System.Random
import qualified Data.Array as A
import System.Process(system)
import Foreign.Storable(Storable)
import Text.Printf(printf)

type Mat = Matrix Double
type Vec = Vector Double

vec :: [Double] -> Vec
vec = fromList

mat :: [[Double]] -> Mat
mat = fromLists

diagl :: [Double] -> Mat
diagl = diag . vec

type Seed = Int

debug :: (Show a) => String -> (a1 -> a) -> a1 -> a1
debug msg f x = trace (light (msg ++ ": " ++ show (f x))) x
    where light s = "\^[[2m"++s++"\^[[0m"

-- | used to avoid incomplete patterns
impossible :: String -> a
impossible msg = error ("impossible input in "++ msg)

-- | stop the program if something is wrong
assert :: Show a => String -> (a -> Bool) -> a -> a
assert msg cond x = if cond x then x else error $ msg ++ show x

splitEvery :: Int -> [t] -> [[t]]
splitEvery _ [] = []
splitEvery k l = take k l : splitEvery k (drop k l)

pairsWith :: (b -> b -> a) -> [b] -> [a]
pairsWith _ [] = []
pairsWith _ [_] = []
pairsWith f (x:xs) = map (f x) xs ++ pairsWith f xs

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

-- | minimum, q0.25, q0.5, q.075, maximum
quartiles :: (Ord a) =>  [a] -> (a,a,a,a,a)
quartiles l = (f 0, f a, f b, f c, f n) where
    f = (s!!)
    s = sort l
    n = length l - 1
    n' = fromIntegral n
    [a,b,c] = map (round' . (*n')) [0.25,0.5,0.75::Double]

-- | display with name and format the mean, min, max, and quartiles of a list of data
shDist :: String -> String -> String -> [Double] -> IO ()
shDist name fmtm fmt xs = printf (name ++ fmtm ++" ("++fmt++", "++fmt++", "++fmt++", "++fmt++", "++fmt++")\n") m xmin a b c xmax
    where (xmin,a,b,c,xmax) = quartiles xs
          m = mean xs


arrayOf :: [a] -> (Int -> a)
arrayOf xs = (A.listArray (0, length xs -1) xs A.!)

myintersect' :: (Ord a) => [a] -> [a] -> [a]
myintersect' xs ys = go xs ys [] where
    go [] _ x = x
    go _ [] x = x
    go (a:as) (b:bs) x
        | a < b = go (a:as) bs x
        | a > b = go as (b:bs) x
        | otherwise = go as bs (a:x)

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


-- | select elements in a list at given positions
selectPos :: (Num a, Enum a) => [a] -> [b] -> [b]
selectPos is = map snd . filter (flip elem is . fst) . zip [0 ..]


-- | specialized @(^2)@
sqr :: Num a => a -> a
sqr x = x^(2::Int)

-- | specialized 'round'
round' :: RealFrac a => a -> Int
round' = round

-- | conversion factor from degrees to radians
degree :: Double
degree = pi/180

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

-- | load an audio file (currently using sox, TO DO: read sample rate)
loadAudio :: FilePath -> IO Mat
loadAudio path = do
    let f = path ++ ".txt"
    _ <- system $ "sox "++path ++" -t dat - | sed '/;/d' - > "++f
    r <- loadMatrix f
    _ <- system $ "rm " ++ f
    return r

-- | save a matrix to an audio file and play it (using sox's play)
savePlayAudio :: Int -> Mat -> FilePath -> IO ()
savePlayAudio rate m path = do
    saveMatrix path "%f" m
    _ <- system $ "play -r "++show rate++" -v 0.5 "++path
    return ()

unliftVector :: Storable a => (Vector a -> Vector a) -> ([a] -> [a])
unliftVector f = toList . f . fromList

unliftRow :: Element a => (Matrix a -> Matrix a) -> (Vector a -> Vector a)
unliftRow f = flatten . f . asRow




-- | Obtains a vector in the same direction with 2-norm=1
unitary :: Vec -> Vec
unitary v = v / scalar (norm v)




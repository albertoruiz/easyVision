{-# LANGUAGE FlexibleContexts #-}

-- | General purpose functions

module Util.Misc(
    -- * Debug
    dimString, redString, errMsg, debug, debugMat,
    impossible, assert, warning,
    -- * List utilities
    splitEvery, pairsWith, posMax, posMin, replaceAt, rep, selectPos, rotateLeft, sliding, slidingPeriodic, subListsBy,
    intersectSorted, unionSort,
    randomPermutation, randomSamples,
    -- * Useful constants
    degree, stdpix, Seed,
    -- * Linear Algebra
    Mat, mat, Vec, vec,
    unliftVector, unliftRow,
    norMax, nulln, orthm,
    -- * Simple statistics
    mean, median, quartiles, shDist,
    -- * Misc
    angleDiff,
    lambdify,
    arrayOf, memo
) where


import Numeric.LinearAlgebra hiding (i)
import Debug.Trace
import Data.Function(on)
import Data.List(elemIndex, sortBy, sort, group, isPrefixOf, tails)
import System.Random
import qualified Data.Array as A
import Foreign.Storable(Storable)
import Text.Printf(printf)
import Data.Array(listArray,(!))
import qualified Data.Vector as V
import System.IO(hPutStrLn, stderr)
import System.Time
import System.Locale


type Mat = Matrix Double
type Vec = Vector Double

vec :: [Double] -> Vec
vec = fromList

mat :: [[Double]] -> Mat
mat = fromLists

type Seed = Int

debug :: (Show a) => String -> (a1 -> a) -> a1 -> a1
debug msg f x = trace (dimString (msg ++ ": " ++ show (f x))) x

dimString :: String -> String
dimString s = "\^[[2m"++s++"\^[[0m"

redString :: String -> String
redString s = "\^[[0;31m"++s++"\^[[0m"

errMsg :: String -> IO ()
errMsg = hPutStrLn stderr . dimString

debugMat :: String -> Int -> (t -> Mat) -> t -> t
debugMat msg dec f x = trace (dimString (msg ++ " " ++ init (dispf dec (f x)))) x

-- | used to avoid incomplete patterns
impossible :: String -> a
impossible msg = error ("impossible input in "++ msg)

-- | stop the program if something is wrong
assert :: Bool -> String -> a -> a
assert cnd msg x = if cnd then x else error msg

warning :: Bool -> String -> a -> a
warning cnd msg x | cnd = trace ("WARNING: "++msg) x
                  | otherwise = x

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

-- | returns the position of the minimum element.
posMin :: Ord a => [a] -> Int
posMin l = p where
    Just p = elemIndex (minimum l) l

-- | pseudorandom permutation of a list
randomPermutation :: Int -> [a] -> [a]
randomPermutation seed l = map fst $ sortBy (compare `on` snd) randomTuples where
    randomTuples = zip l (randomRs (0, 1::Double) (mkStdGen seed))

-- | without replacement
randomSamples :: Seed -> Int -> [a] -> [[a]]
randomSamples seed n dat = map (V.toList . V.backpermute vdat . V.fromList) goodsubsets
  where
    
    randomIndices = randomRs (0, length dat -1) (mkStdGen seed)
    goodsubsets = filter unique $ splitEvery n randomIndices

    vdat = V.fromList dat
    
    unique = g . sort
    g []  = True
    g [_] = True
    g (a:b:cs) | a == b    = False
               | otherwise = unique (b:cs)



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
    [a,b,c] = map (round . (*n')) [0.25,0.5,0.75::Double]

-- | display with name and format the mean, min, max, and quartiles of a list of data
shDist :: String -> String -> String -> [Double] -> IO ()
shDist name fmtm fmt xs = printf (name ++ fmtm ++" ("++fmt++", "++fmt++", "++fmt++", "++fmt++", "++fmt++")\n") m xmin a b c xmax
    where (xmin,a,b,c,xmax) = quartiles xs
          m = mean xs


arrayOf :: [a] -> (Int -> a)
arrayOf xs = (A.listArray (0, length xs -1) xs A.!)

-- | memoize a function for arguments in [-a,a]
memo :: Int -> (Int -> a) -> (Int -> a)
memo mx f = g where
    m = listArray (-mx,mx::Int) [f k | k <- [-mx..mx]]
    g w = m ! w


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

-- | replace substring
rep :: Eq a => ([a], [a]) -> [a] -> [a]
rep (_,_) [] = []
rep (c,r) f@(x:xs) 
  | c `isPrefixOf` f = r ++ rep (c,r) (drop (length c) f)
  | otherwise        = x:(rep (c,r) xs)


-- | select elements in a list at given positions
selectPos :: (Num a, Enum a, Eq a) => [a] -> [b] -> [b]
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
orthm = fromColumns . orth

--------------------------------------------------------------------------------

formattedTime :: IO String
formattedTime = do
    t <- getClockTime >>= toCalendarTime
    return $ formatCalendarTime
               defaultTimeLocale
               (iso8601DateFormat (Just "%H-%M-%S"))
               t


formattedDate :: IO String
formattedDate = do
    t <- getClockTime >>= toCalendarTime
    return $ formatCalendarTime
               defaultTimeLocale
               (iso8601DateFormat Nothing)
               t


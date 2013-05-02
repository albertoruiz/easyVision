{-# LANGUAGE FlexibleContexts #-}

-- | General purpose functions

module Util.Misc where

import Numeric.LinearAlgebra hiding (i)
import Debug.Trace
import Data.Function(on)
import Data.List(elemIndex, sortBy, sort, group, isPrefixOf, tails)
import System.Random
import qualified Data.Array as A
import System.Process(system)
import Foreign.Storable(Storable)
import Text.Printf(printf)
import Data.Array(listArray,(!))
import qualified Numeric.LinearAlgebra.Util as U

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
debug msg f x = trace (dimString (msg ++ ": " ++ show (f x))) x

dimString :: String -> String
dimString s = "\^[[2m"++s++"\^[[0m"

redString :: String -> String
redString s = "\^[[0;31m"++s++"\^[[0m"

debugMat :: String -> Int -> (t -> Mat) -> t -> t
debugMat msg dec f x = trace (dimString (msg ++ " " ++ init (dispf dec (f x)))) x

-- | used to avoid incomplete patterns
impossible :: String -> a
impossible msg = error ("impossible input in "++ msg)

-- | stop the program if something is wrong
assert :: Show a => String -> (a -> Bool) -> a -> a
assert msg cnd x = if cnd x then x else error $ msg ++ show x

warning :: (a -> Bool) -> String -> a -> a
warning cnd msg x | cnd x = debug msg (const ()) x --FIXME
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
loadAudio fpath = do
    let f = fpath ++ ".txt"
    _ <- system $ "sox "++fpath ++" -t dat - | sed '/;/d' - > "++f
    r <- loadMatrix f
    _ <- system $ "rm " ++ f
    return r

-- | save a matrix to an audio file and play it (using sox's play)
savePlayAudio :: Int -> Mat -> FilePath -> IO ()
savePlayAudio rate m fpath = do
    saveMatrix fpath "%f" m
    _ <- system $ "play -r "++show rate++" -v 0.5 "++fpath
    return ()

unliftVector :: Storable a => (Vector a -> Vector a) -> ([a] -> [a])
unliftVector f = toList . f . fromList

unliftRow :: Element a => (Matrix a -> Matrix a) -> (Vector a -> Vector a)
unliftRow f = flatten . f . asRow


-- | Obtains a vector in the same direction with 2-norm=1
unitary :: Vec -> Vec
unitary v = v / scalar (norm v)

-- | Matrix of pairwise squared distances of row vectors
-- (using the matrix product trick in blog.smola.org)
pairwiseD2 :: Mat -> Mat -> Mat
pairwiseD2 x y | ok = x2 `outer` oy + ox `outer` y2 - 2* x <> trans y
               | otherwise = error $ "pairwiseD2 with different number of columns: "
                                   ++ show (size x) ++ ", " ++ show (size y)
  where
    ox = ones (rows x)
    oy = ones (rows y)
    oc = ones (cols x)
    ones k = constant 1 k
    x2 = sqr x <> oc
    y2 = sqr y <> oc
    ok = cols x == cols y

size :: Matrix t -> (Int, Int)
size m = (rows m, cols m)

----------------------------------------------------------------------

-- spanning tree of a graph
-- see also http://stackoverflow.com/questions/4290163/how-can-i-write-a-mst-algorithm-prim-or-kruskal-in-haskell


-- | Minimum/maximum spanning tree, from sorted arcs
-- | nmax is given by the caller to keep laziness
kruskal :: Int -> [(Int, Int)] -> [(Int, Int)]
kruskal nmax s = fst $ myfoldl' ((nmax==).length.fst) f ([],r0) s where
    -- nmax = maximum (map snd s)
    r0 = map return [0..nmax]
    f (g,r) (i,j) = if i `elem` r!!j then (g,r) else ((i,j):g, r')
        where r' = replaceAt z (replicate (length z) z) r
              z = unionSort [r!!i, r!!j]


-- TO DO: use scanl and takeWhile
myfoldl' :: (a -> Bool) -> (a -> t -> a) -> a -> [t] -> a
myfoldl' done f z0 xs0 = lgo z0 xs0
    where lgo z _ | done z = z
          lgo z []     = z
          lgo z (x:xs) = let z' = f z x in z' `seq` lgo z' xs    


neigh :: (Ord t) => [(t, t)] -> t -> [t]
neigh g n = unionSort [[ j | (k,j) <- g , k == n ],
                      [ i | (i,k) <- g , k == n ]]


--path from a to b in a tree
path :: [(Int,Int)] -> Int -> Int -> [Int]
path g a b | b `elem` na = [a,b]
           | null subs = []
           | otherwise = a : head subs
  where
    na = neigh g a
    g' = filter noa g where noa (i,j) = i /= a && j /= a
    subs = filter (not.null) [ path g' v b | v <- na ]


treeGroups :: Double -> (a -> a -> Double) -> [a] -> [[a]]
treeGroups th dist xs = gs
  where
    nmx = length xs - 1
    t = kruskal nmx arcs
    arcs = map snd $ filter ((<th).fst) $ sort $ pairsWith f (xs `zip` [0..])
    f (x,i) (y,j) = (dist x y, (i,j))
    con = connected nmx t
    gs = map (map (xs!!)) con


connected :: Int -> [(Int, Int)] -> [[Int]]
connected nmax arcs = fixedPoint grow (map return [0..nmax])
  where
    fixedPoint f x0 = let x1 = f x0 in if x1 == x0 then x0 else fixedPoint f x1
    grow = unique . map (unique . concatMap neigh')
    neigh' x = x : neigh arcs x
    unique :: Ord t => [t] -> [t]
    unique = map head . group . sort

----------------------------------------------------------------------

-- | Taken from \"not-in-base\" package.
lambdify :: (a -> b -> t) -> (c -> a) -> (c -> b) -> c -> t
lambdify f a b x = f (a x) (b x)

--------------------------------------------------------------------

mt :: Mat -> Mat
mt = trans . inv

----------------------------------------------------------------------

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

diagBlock :: (Num (Vector t), Container Vector t)
          => [Matrix t] -> Matrix t
diagBlock ms = fromBlocks $ zipWith f ms [0..]
  where
    f m k = take n $ replicate k 0 ++ m : repeat 0
    n = length ms

--------------------------------------------------------------------------------

norMax :: (Num (c e), Container c e) => e -> c e -> c e
norMax s m = m * scalar (s / v)
  where
    p = maxIndex (abs m)
    v = m `atIndex` p

--------------------------------------------------------------------------------

null1 :: Matrix Double -> Vector Double
null1 = last . toColumns . snd . rightSV

nulln :: Int -> Matrix Double -> [Vector Double]
nulln n = reverse . take n . reverse . toColumns . snd . rightSV

null1eig :: Matrix Double -> Vector Double
null1eig = last . toColumns . snd . eigSH

orthm :: Matrix Double -> Matrix Double
orthm = fromColumns . orth

rowOuters :: Matrix Double -> Matrix Double -> Matrix Double
rowOuters a b = a' * b'
  where
    a' = kronecker a (U.ones 1 (cols b))
    b' = kronecker (U.ones 1 (cols a)) b


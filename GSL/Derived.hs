{-# OPTIONS -fglasgow-exts #-}

module GSL.Derived where

import GSL.Base
import GSL.Wrappers
import Complex
import Data.List(transpose,intersperse)
import Numeric(showGFloat)
import Foreign

partit :: Int -> [a] -> [[a]]
partit _ [] = []
partit n l  = take n l : partit n (drop n l)


class Trans t where
    -- | transpose of a matrix
    trans :: Matrix t -> Matrix t
        
instance Trans Double where
    trans = transR
    
instance Trans (Complex Double) where
    trans = transC
    
-- | creates a matrix from a list of lists (considered as rows). Related functions: 'realMatrix', 'matrix' and 'fromRows'.
fromLists :: Storable t => [[t]] -> Matrix t
fromLists = fromRows . map fromList 

-- | creates a list of lists from the rows o a matrix
toLists :: Storable t => Matrix t -> [[t]] 
toLists = map toList . toRows

-- | creates a matrix from a list of vectors, as columns
fromCols :: (Storable t, Trans t) => [Vector t] -> Matrix t
fromCols m = trans . fromRows $ m

-- | creates a list of vectors from the columns of a matrix
toCols :: (Storable t, Trans t) => Matrix t -> [Vector t]
toCols m = toRows . trans $ m

-- | creates a matrix from a vertical list of matrices
joinVert :: (Storable t) => [Matrix t] -> Matrix t
joinVert ms = case common cols ms of
    Nothing -> error "joinVert on matrices with different number of columns"
    Just c  -> reshape c $ join (map flatten ms)

-- | creates a matrix from a horizontal list of matrices
joinHoriz :: (Storable t, Trans t) => [Matrix t] -> Matrix t
joinHoriz ms = trans. joinVert . map trans $ ms

{- | creates a matrix from blocks given as a list of lists of matrices

> > let a = diag $ fromList [5,7,2::Double]
> > let b = constant (-1) (3::Int,4::Int)
> > fromBlocks [[a,b],[b,a]]
>  5.  0.  0. -1. -1. -1. -1.
>  0.  7.  0. -1. -1. -1. -1.
>  0.  0.  2. -1. -1. -1. -1.
> -1. -1. -1. -1.  5.  0.  0.
> -1. -1. -1. -1.  0.  7.  0.
> -1. -1. -1. -1.  0.  0.  2.

-}
fromBlocks :: (Storable t, Trans t) => [[Matrix t]] -> Matrix t
fromBlocks = joinVert . map joinHoriz 

-- | obtains the complex conjugate of a complex vector
conjV :: CV -> CV
conjV v = asComplex $ flatten $ reshape 2 (asReal v) `multiply` diagR (fromList [1,-1])

-- | dot product of two real vectors
vRvR :: V -> V -> Double
vRvR u v = r where
    [[r]] = toLists $ multiply (reshape (size u) u) (reshape 1 v) 

-- | dot product of two complex vectors
vCvC :: CV -> CV -> Complex Double
vCvC u v = r where
    [[r]] = toLists $ multiplyC (reshape (size u) u) (reshape 1 v)

-- | real matrix vector product 
mRvR :: M -> V -> V
mRvR m v = flatten $ multiply m (reshape 1 v) 

-- | complex matrix vector product 
mCvC :: CM -> CV -> CV
mCvC m v = flatten $ multiplyC m (reshape 1 v) 

-- | real vector matrix product 
vRmR :: V -> M -> V
vRmR v m = flatten $ multiply (reshape (size v) v) m 

-- | complex vector matrix product 
vCmC :: CV -> CM -> CV
vCmC v m = flatten $ multiplyC (reshape (size v) v) m 

scaleC :: Complex Double -> CV -> CV
scaleC x = fromList . map (*x). toList

offsetC :: Complex Double -> CV -> CV
offsetC x = fromList . map (+x). toList

norm2 :: V -> Double
norm2 = toScalar 1 

norm1 :: V -> Double
norm1 = toScalar 2 

normInf :: V -> Double
normInf = toScalar 4 

constantM :: Double -> Int -> Int -> M
constantM val r c = reshape c $ constantV val (r*c)

-- | creates a complex vector from vectors with real and imaginary parts
complexV :: (V,V) -> CV
complexV (r,i) = asComplex $ flatten $ fromCols [r,i]

-- | creates a complex matrix from matrices with real and imaginary parts
complexM :: (M,M) -> CM
complexM (r,i) = reshape (cols r) $ asComplex $ flatten $ fromCols [flatten r, flatten i]

-- | extracts the real and imaginary parts of a complex vector
reimV :: CV -> (V,V)
reimV m = (a,b) where [a,b] = toCols $ reshape 2 $ asReal m

-- | extracts the real and imaginary parts of a complex matrix
reimM :: CM -> (M,M)
reimM m = (reshape c a, reshape c b)
    where c = cols m
          [a,b] = toCols $ reshape 2 $ asReal $ flatten m 

--------------------------------------------------------------------

subMatrixC r1 r2 c1 c2 x@(M r c _) = 
    reshape (c2-c1+1) . asComplex . flatten . 
    subMatrixR r1 r2 (2*c1) (2*c2). 
    reshape (2*c) . asReal . flatten $ x

---------------------------------------------------------------------

-- shows a Double with n digits after the decimal point    
shf :: (RealFloat a) => Int -> a -> String     
shf dec n | abs n < 1e-10 = "0."
          | abs (n - (fromIntegral.round $ n)) < 1e-10 = show (round n) ++"."
          | otherwise = showGFloat (Just dec) n ""    
-- shows a Complex Double as a pair, with n digits after the decimal point    
shfc n z@ (a:+b) 
    | magnitude z <1e-10 = "0."
    | abs b < 1e-10 = shf n a
    | abs a < 1e-10 = shf n b ++"i"
    | b > 0         = shf n a ++"+"++shf n b ++"i"
    | otherwise     = shf n a ++shf n b ++"i"         

dsp :: String -> [[String]] -> String
dsp sep as = unlines . map unwords' $ transpose mtp where 
    mt = transpose as
    longs = map (maximum . map length) mt
    mtp = zipWith (\a b -> map (pad a) b) longs mt
    pad n str = replicate (n - length str) ' ' ++ str    
    unwords' = concat . intersperse sep

showMatrix :: (Storable t) => String -> (t -> String) -> Matrix t -> String
showMatrix sep f m = dsp sep . map (map f) . toLists $ m

showVector :: (Storable t) => String -> (t -> String) -> Vector t -> String
showVector sep f v = showMatrix sep f (reshape (size v) v)



-----------------------------------------------
instance Read V where
    readsPrec _ s = case reads s of
                [(l,r)] -> [(fromList l,r)]
                _       -> [(fromList . map read . words $ sp, sr)]
      where clean = dropWhile (\c->c==' ' || c=='\n') s
            (sp,sr) = break (\c->c==';' || c=='\n') clean
            

instance Read CV where
    readsPrec _ s = case reads s of
                [(l,r)] -> [(fromList l,r)]
                _       -> [(fromList . map readComplex . words $ sp,sr)]
      where clean = dropWhile (\c->c==' ' || c=='\n') s
            (sp,sr) = break (\c->c==';' || c=='\n') clean

readComplex :: String -> Complex Double
readComplex s = case reads s of
                [(z,"")] -> z
                _ -> case reads s of
                     [(x,"")] -> x:+0
                     _ -> case reads s of
                          [((x,y),"")] -> x:+y
                          _ -> error  "parsing a complex number"

instance Read M where
    readsPrec _ s = case reads s of
                [(ll,r)] -> [(fromLists ll,r)]
                _ -> [(readMatrix read s,"")]

instance Read CM where
    readsPrec _ s = case reads s of
                [(ll,r)] -> [(fromLists ll,r)]
                _ -> [(readMatrix readComplex s,"")]



readVector :: (Read t, Storable t) => String -> Vector t
readVector v = fromList . read $ v 

readMatrix r = fromLists . map (map r). map words . lines . cleanpun   

fromFile :: FilePath -> IO M
fromFile filename = readFile filename >>= return . readMatrix read

cleanpun = map f where
    f ',' = ' '
    f ';' = '\n'
    f x = x            
    
-----------------------------------------------------    
    
{- | The Hilbert matrix of order N.  The i, j element of a Hilbert matrix is defined as 1 \/ (i + j - 1) (taken from GNU-octave)

> > hilb 5
>    1. 0.500 0.333 0.250 0.200
> 0.500 0.333 0.250 0.200 0.167
> 0.333 0.250 0.200 0.167 0.143
> 0.250 0.200 0.167 0.143 0.125
> 0.200 0.167 0.143 0.125 0.111

-}
hilb :: Int -> M
hilb n = reshape n $ fromList [1/fromIntegral (i+j-1)| i<-[1..n], j<-[1..n]]
    
{- | The identity matrix of order N.

> > ident 4
> 1. 0. 0. 0.
> 0. 1. 0. 0.
> 0. 0. 1. 0.
> 0. 0. 0. 1.

-}
ident :: Int -> M
ident = diagR . constantV 1
  
-----------------------------------------------------------

isSquare (M r c _) = r==c

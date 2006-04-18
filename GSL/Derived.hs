{-# OPTIONS -O -fglasgow-exts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LinearAlgebra.Derived
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL-style
-- 
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- Functions that can be written in terms of the most basic ones.
--
-----------------------------------------------------------------------------

module GSL.Derived where

import GSL.Core
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
    trans :: GSLMatrix t -> GSLMatrix t
        
instance Trans Double where
    trans = transR
    
instance Trans (Complex Double) where
    trans = transC
    
-- | Creates a matrix from a list of lists (considered as rows). Related functions: 'GSL.Interface.realMatrix', 'GSL.Interface.complexMatrix', 'fromRows', 'fromColumns', 'fromStorableArrayM', 'fromFile', and 'gslReadMatrix'.
fromList2 :: Storable t => [[t]] -> GSLMatrix t
fromList2 = fromRows . map fromList1 

-- | Creates a list of lists from the rows o a matrix
toList2 :: Storable t => GSLMatrix t -> [[t]] 
toList2 = map toList1 . toRows

-- | Creates a matrix from a list of vectors, as columns
fromColumns :: (Storable t, Trans t) => [GSLVector t] -> GSLMatrix t
fromColumns m = trans . fromRows $ m

-- | Creates a list of vectors from the columns of a matrix
toColumns :: (Storable t, Trans t) => GSLMatrix t -> [GSLVector t]
toColumns m = toRows . trans $ m

-- | creates a matrix from a vertical list of matrices
joinVert :: (Storable t) => [GSLMatrix t] -> GSLMatrix t
joinVert ms = case common cols ms of
    Nothing -> error "joinVert on matrices with different number of columns"
    Just c  -> reshape c $ join (map flatten ms)

-- | creates a matrix from a horizontal list of matrices
joinHoriz :: (Storable t, Trans t) => [GSLMatrix t] -> GSLMatrix t
joinHoriz ms = trans. joinVert . map trans $ ms

{- | Creates a matrix from blocks given as a list of lists of matrices:

@\> let a = 'GSL.Interface.diag' $ 'GSL.Interface.realVector' [5,7,2]
\> let b = 'GSL.Interface.constant' (-1) (3::Int,4::Int)
\> fromBlocks [[a,b],[b,a]]
 5.  0.  0. -1. -1. -1. -1.
 0.  7.  0. -1. -1. -1. -1.
 0.  0.  2. -1. -1. -1. -1.
-1. -1. -1. -1.  5.  0.  0.
-1. -1. -1. -1.  0.  7.  0.
-1. -1. -1. -1.  0.  0.  2.@

-}
fromBlocks :: (Storable t, Trans t) => [[GSLMatrix t]] -> GSLMatrix t
fromBlocks = joinVert . map joinHoriz 

-- | obtains the complex conjugate of a complex vector
conjV :: ComplexVector -> ComplexVector
conjV v = asComplex $ flatten $ reshape 2 (asReal v) `multiply` diagR (fromList1 [1,-1])

-- | dot product of two real vectors
vRvR :: Vector -> Vector -> Double
vRvR u v = r where
    [[r]] = toList2 $ multiply (reshape (size u) u) (reshape 1 v) 

-- | dot product of two complex vectors
vCvC :: ComplexVector -> ComplexVector -> Complex Double
vCvC u v = r where
    [[r]] = toList2 $ multiplyC (reshape (size u) u) (reshape 1 v)

-- | real matrix vector product 
mRvR :: Matrix -> Vector -> Vector
mRvR m v = flatten $ multiply m (reshape 1 v) 

-- | complex matrix vector product 
mCvC :: ComplexMatrix -> ComplexVector -> ComplexVector
mCvC m v = flatten $ multiplyC m (reshape 1 v) 

-- | real vector matrix product 
vRmR :: Vector -> Matrix -> Vector
vRmR v m = flatten $ multiply (reshape (size v) v) m 

-- | complex vector matrix product 
vCmC :: ComplexVector -> ComplexMatrix -> ComplexVector
vCmC v m = flatten $ multiplyC (reshape (size v) v) m 

scaleC :: Complex Double -> ComplexVector -> ComplexVector
scaleC x = fromList1 . map (*x). toList1

offsetC :: Complex Double -> ComplexVector -> ComplexVector
offsetC x = fromList1 . map (+x). toList1

norm2 :: Vector -> Double
norm2 = toScalar 1 

norm1 :: Vector -> Double
norm1 = toScalar 2 

normInf :: Vector -> Double
normInf = toScalar 4 

constantM :: Double -> Int -> Int -> Matrix
constantM val r c = reshape c $ constantV val (r*c)

-- | creates a complex vector from vectors with real and imaginary parts
complexV :: (Vector,Vector) -> ComplexVector
complexV (r,i) = asComplex $ flatten $ fromColumns [r,i]

-- | creates a complex matrix from matrices with real and imaginary parts
complexM :: (Matrix,Matrix) -> ComplexMatrix
complexM (r,i) = reshape (cols r) $ asComplex $ flatten $ fromColumns [flatten r, flatten i]

-- | extracts the real and imaginary parts of a complex vector
reimV :: ComplexVector -> (Vector,Vector)
reimV m = (a,b) where [a,b] = toColumns $ reshape 2 $ asReal m

-- | extracts the real and imaginary parts of a complex matrix
reimM :: ComplexMatrix -> (Matrix,Matrix)
reimM m = (reshape c a, reshape c b)
    where c = cols m
          [a,b] = toColumns $ reshape 2 $ asReal $ flatten m 

--------------------------------------------------------------------

subMatrixC (r0,c0) (rt,ct) x@(M r c _) = 
    reshape ct . asComplex . flatten . 
    subMatrixR (r0,2*c0) (rt,2*ct) .
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

showMatrix :: (Storable t) => String -> (t -> String) -> GSLMatrix t -> String
showMatrix sep f m = dsp sep . map (map f) . toList2 $ m

showVector :: (Storable t) => String -> (t -> String) -> GSLVector t -> String
showVector sep f v = showMatrix sep f (reshape (size v) v)



-----------------------------------------------
instance Read Vector where
    readsPrec _ s = case reads s of
                [(l,r)] -> [(fromList1 l,r)]
                _       -> [(fromList1 . map read . words $ sp, sr)]
      where clean = dropWhile (\c->c==' ' || c=='\n') s
            (sp,sr) = break (\c->c==';' || c=='\n') clean
            

instance Read ComplexVector where
    readsPrec _ s = case reads s of
                [(l,r)] -> [(fromList1 l,r)]
                _       -> [(fromList1 . map readComplex . words $ sp,sr)]
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

instance Read Matrix where
    readsPrec _ s = case reads s of
                [(ll,r)] -> [(fromList2 ll,r)]
                _ -> [(readMatrix read s,"")]

instance Read ComplexMatrix where
    readsPrec _ s = case reads s of
                [(ll,r)] -> [(fromList2 ll,r)]
                _ -> [(readMatrix readComplex s,"")]



readVector :: (Read t, Storable t) => String -> GSLVector t
readVector v = fromList1 . read $ v 

readMatrix r = fromList2 . map (map r). map words . lines . cleanpun   

-- | Loads a real matrix from a formatted ASCII text file 
fromFile :: FilePath -> IO Matrix
fromFile filename = readFile filename >>= return . readMatrix read

-- | Saves a real matrix to a formatted ascii text file
toFile :: FilePath -> Matrix -> IO ()
toFile filename matrix = writeFile filename (unlines . map unwords. map (map show) . toList2 $ matrix)

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
hilb :: Int -> Matrix
hilb n = reshape n $ fromList1 [1/fromIntegral (i+j-1)| i<-[1..n], j<-[1..n]]
    
{- | The identity matrix of order N.

> > ident 4
> 1. 0. 0. 0.
> 0. 1. 0. 0.
> 0. 0. 1. 0.
> 0. 0. 0. 1.

-}
ident :: Int -> Matrix
ident = diagR . constantV 1
  
-----------------------------------------------------------

isSquare (M r c _) = r==c

{- | creates real vector containing a range of values:

> > linspace 10 (-2,2)
>-2. -1.556 -1.111 -0.667 -0.222 0.222 0.667 1.111 1.556 2.

-}
linspace :: Int -> (Double, Double) -> GSLVector Double
linspace n (a,b) = fromList1 [a::Double,a+delta .. b] 
    where delta = (b-a)/(fromIntegral n -1)
    
    
-------------------------------------------------------------
    
{- | Machine precision of a Double.     

>> eps
> 2.22044604925031e-16

(The value used by GNU-Octave)

-}
eps :: Double
eps =  2.22044604925031e-16

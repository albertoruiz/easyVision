{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSL.Matrix
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Basic operations on matrices

(In construction...)

-}
-----------------------------------------------------------------------------
module GSL.Matrix (
    GSLMatrix,
    rows, cols,
    (!!:),
    GSL.Matrix.fromList, GSL.Matrix.toList,
    fromArray, toArray,
    fromRows, toRows,
    fromColumns, toColumns,
    reshape, flatten,
    fromBlocks,
    GSL.Matrix.toComplex, GSL.Matrix.fromComplex,
    Field(..)
) where

import GSL.GSL
import GSL.Internal
import Foreign
import Complex
import Data.List(transpose,intersperse)
import Numeric(showGFloat)
import Data.Array.Storable
import Data.Array(Array)
import GSL.Vector as V
import Text.Printf
import Foreign.C.String


-- | Reading a matrix position.
(!!:) :: (Storable t) => GSLMatrix t -> (Int,Int) -> t
infixl 9 !!: 
(M r c p) !!: (i,j) 
    | i<0 || i>=r || j<0 || j>=c = error "matrix indexing out of range"
    | otherwise   = unsafePerformIO $ do
                        withForeignPtr p $ \p ->
                            peek (advancePtr p (i*c+j)) 


class Storable t => Field t where
    -- | transpose of a matrix
    trans :: GSLMatrix t -> GSLMatrix t
    subMatrix :: (Int,Int)-> (Int,Int) -> GSLMatrix t -> GSLMatrix t

instance Field Double where
    trans = transR
    subMatrix = subMatrixR

instance Field (Complex Double) where
    trans = transC
    subMatrix = subMatrixC



-- | Creates a matrix from a list of vectors, as columns
fromColumns :: (Field t) => [GSLVector t] -> GSLMatrix t
fromColumns m = trans . fromRows $ m

-- | Creates a list of vectors from the columns of a matrix
toColumns :: (Field t) => GSLMatrix t -> [GSLVector t]
toColumns m = toRows . trans $ m

-- | creates a matrix from a vertical list of matrices
joinVert :: (Storable t) => [GSLMatrix t] -> GSLMatrix t
joinVert ms = case common cols ms of
    Nothing -> error "joinVert on matrices with different number of columns"
    Just c  -> reshape c $ join (map flatten ms)

-- | creates a matrix from a horizontal list of matrices
joinHoriz :: (Field t) => [GSLMatrix t] -> GSLMatrix t
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
fromBlocks :: (Field t) => [[GSLMatrix t]] -> GSLMatrix t
fromBlocks = joinVert . map joinHoriz 


-- | transpose of complex matrix
transC :: GSLMatrix (Complex Double) -> GSLMatrix (Complex Double)
transC x@(M r c p) = unsafePerformIO $ do
    q <- mallocForeignPtrArray (r*c)
    withForeignPtr p $ \p ->
        withForeignPtr q $ \q ->
            prot "transR" $ c_transC r c p c r q
    return (M c r q)

-- | extraction of a submatrix of a real matrix
subMatrixR :: (Int,Int) -- ^ (r0,c0) starting position 
           -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
           -> GSLMatrix Double -> GSLMatrix Double
subMatrixR (r0,c0) (rt,ct) x@(M r c p) = unsafePerformIO $ do
    q <- mallocForeignPtrArray (r*c)
    withForeignPtr p $ \p ->
        withForeignPtr q $ \q ->
            prot "subMatrixR" $ c_submatrixR r0 (r0+rt-1) c0 (c0+ct-1) r c p rt ct q
    return (M rt ct q)

subMatrixC (r0,c0) (rt,ct) x@(M r c _) = 
    reshape ct . asComplex . flatten .
    subMatrixR (r0,2*c0) (rt,2*ct) .
    reshape (2*c) . asReal . flatten $ x


-- | Creates a matrix from a list of lists (considered as rows). Related functions: 'GSL.Interface.realMatrix', 'GSL.Interface.complexMatrix', 'fromRows', 'fromColumns', 'fromStorableArrayM', 'fromFile', and 'gslReadMatrix'.
fromList :: Storable t => [[t]] -> GSLMatrix t
fromList = fromRows . map V.fromList

-- | Creates a list of lists from the rows o a matrix
toList :: Storable t => GSLMatrix t -> [[t]]
toList = map V.toList . toRows

-- | creates a complex matrix from matrices with real and imaginary parts
toComplex :: (GSLMatrix Double, GSLMatrix Double) ->  GSLMatrix (Complex Double)
toComplex (r,i) = reshape (cols r) $ asComplex $ flatten $ fromColumns [flatten r, flatten i]

-- | extracts the real and imaginary parts of a complex matrix
fromComplex :: GSLMatrix (Complex Double) -> (GSLMatrix Double, GSLMatrix Double)
fromComplex m = (reshape c a, reshape c b)
    where c = cols m
          [a,b] = toColumns $ reshape 2 $ asReal $ flatten m 

----------------------------------------------------------------------


--disp :: Int -> UMatrix -> IO ()
--disp n = putStrLn . showUMatrix " " (shf n)

--disp' :: Int -> UCMatrix -> IO ()
--disp' n = putStrLn . showUCMatrix " | " (shfc n)



----------------------------------------------------------------------
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

--showUMatrix sep f m = dsp sep . partit (cols m) . map f . elems $ m
--showUCMatrix sep f m = dsp sep . partit (cols' m) . map f . cosa . elems $ m


-- | loads a matrix efficiently from formatted ASCII text file (the number of rows and columns must be known in advance).
fscanf :: FilePath -> (Int,Int) -> IO (GSLMatrix Double)
fscanf filename (r,c) = do
    charname <- newCString filename
    m <- mallocForeignPtrArray (r*c)
    withForeignPtr m $ \pm ->
        prot "gslReadMatrix" $ c_gslReadMatrix charname r c pm
    --free charname  -- TO DO: free the auxiliary CString
    return (M r c m)

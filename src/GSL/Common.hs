{-# OPTIONS #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GSL.Common
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL-style
-- 
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- basic definitions useful for both vectors and matrices
--
-----------------------------------------------------------------------------

-- #hide

module GSL.Common where

import Foreign
import GSL.Types
import GSL.Wrappers
import Data.Array.Storable
import Data.Array(Array)

partit :: Int -> [a] -> [[a]]
partit _ [] = []
partit n l  = take n l : partit n (drop n l)

-- | obtains the common value of a property of a list
common :: (Eq a) => (b->a) -> [b] -> Maybe a
common f = commonval . map f where
    commonval :: (Eq a) => [a] -> Maybe a
    commonval [] = Nothing
    commonval [a] = Just a
    commonval (a:b:xs) = if a==b then commonval (b:xs) else Nothing


-- | Creates a 'Vector' from a list.
fromList :: Field a => [a] -> Vector a
fromList [] = error "trying to create an empty GSL vector"
fromList l = unsafePerformIO $ do
    let n = length l
    p <- mallocForeignPtrArray n
    withForeignPtr p $ \p ->
        pokeArray p l
    return (V n p)

-- | The inverse of 'fromList'.
toList :: Field t => Vector t -> [t]
toList (V n p) = unsafePerformIO $ withForeignPtr p $ peekArray n

-- | creates a Vector taking a number of consecutive toList from another Vector
subVector :: Field t => Int       -- ^ index of the starting element
                     -> Int       -- ^ number of toList to extract
                     -> Vector t  -- ^ source
                     -> Vector t  -- ^ result
subVector k l (V n p)
    | k<0 || k >= n || k+l > n || l < 0 = error "subVector out of range"
    | otherwise = unsafePerformIO $ do
        q <- mallocForeignPtrArray l
        withForeignPtr p $ \p ->
            withForeignPtr q $ \ q ->
                copyArray q (advancePtr p k) l
        return (V l q)

-- | creates a new Vector by joining a list of Vectors
join :: Field t => [Vector t] -> Vector t
join [] = error "joining an empty list"
join as = unsafePerformIO $ do
    let tot = sum (map size as)
    p <- mallocForeignPtrArray tot
    withForeignPtr p $ \p ->
        joiner as tot p
    return (V tot p)
  where joiner [] _ _ = return ()
        joiner (V n b : cs) _ p = do
            withForeignPtr b  $ \b' -> copyArray p b' n
            joiner cs 0 (advancePtr p n)

-- | adapts a function on vectors to work on all the toList of matrices
asVector :: (Vector a -> Vector b) -> Matrix a -> Matrix b
asVector f m = reshape (cols m) . f . flatten $ m

sameShape (M r1 c1 _) (M r2 c2 _) = r1==r2 && c1 == c2 

-- | adapts a function on two vectors to work on all the toList of two matrices
asVector2 :: (Vector a -> Vector b -> Vector c) -> Matrix a -> Matrix b -> Matrix c
asVector2 f m1 m2 
    | sameShape m1 m2 = reshape (cols m1) $ f (flatten m1) (flatten m2)
    | otherwise = error "inconsistent matrix dimensions" 

-- | creates a Matrix from a list of vectors
fromRows :: Field t => [Vector t] -> Matrix t
fromRows vs = case common size vs of
    Nothing -> error "fromRows applied to [] or to vectors with different sizes"
    Just c  -> reshape c (join vs)

-- | extracts the rows of a matrix as a list of vectors
toRows :: Field t => Matrix t -> [Vector t]
toRows x = toRows' 0 where
    v = flatten x
    r = rows x
    c = cols x
    toRows' k | k == r*c  = []
              | otherwise = subVector k c v : toRows' (k+c)

class (Storable t) => Field t where
    transM :: Matrix t -> Matrix t
    subMatrixM :: (Int,Int)-> (Int,Int) -> Matrix t -> Matrix t
    diagM :: Vector t -> Matrix t
    takeDiagM :: Matrix t -> Vector t
    multiplyM :: Matrix t -> Matrix t -> Matrix t
    scaleV :: t -> Vector t -> Vector t
    addV :: Vector t -> Vector t -> Vector t
    scaleM :: t -> Matrix t -> Matrix t
    addM :: Matrix t -> Matrix t -> Matrix t
    pnormV :: Int -> Vector t -> Double
    pnormM :: Int -> Matrix t -> Double

instance Field Double where
    transM = transR
    subMatrixM = subMatrixR
    diagM = diagR
    takeDiagM = takeDiagR
    multiplyM = multiplyR
    addV = vectorZip 3
    addM = asVector2 addV
    scaleV = scaleR
    scaleM x = asVector (scaleV x)
    pnormV = pnormRV
    pnormM = pnormRM

instance Field (Complex Double) where
    transM = transC
    subMatrixM = subMatrixC
    diagM = diagC
    takeDiagM = takeDiagC
    multiplyM = multiplyC
    addV = likeReal (vectorZip 3)
    addM = asVector2 addV
    scaleV = scaleC
    scaleM x = asVector (scaleV x)
    pnormV = pnormCV
    pnormM = pnormCM

likeReal f x y = asComplex $ f (asReal x) (asReal y)


diag :: Field a => Vector a -> Matrix a
diag = diagM


takeDiag :: Field a => Matrix a -> Vector a
takeDiag = takeDiagM


class Container t where
    mapG :: (Field a, Field b) => (a ->  b) -> t a ->  t b
    zipG :: (Field a, Field b, Field c) => (a -> b -> c) -> t a -> t b -> t c
    toComplexG :: (t Double, t Double) ->  t (Complex Double)
    fromComplexG :: t (Complex Double) -> (t Double, t Double)
    conjG :: t (Complex Double) -> t (Complex Double)
    asComplexG :: t Double -> t (Complex Double)
    addG :: Field a => t a -> t a -> t a
    scaleG :: Field a => a -> t a -> t a
    pnormG :: Field a => Int -> t a -> Double 

instance Container Vector where
    mapG f = fromList . map f . toList
    zipG f a b = fromList $ zipWith f (toList a) (toList b)
    toComplexG = toComplexV
    fromComplexG = fromComplexV
    conjG = conjV
    asComplexG v = toComplexG (v,constant 0 (size v))
    addG = addV
    scaleG = scaleV
    pnormG = pnormV

instance Container Matrix where
    mapG f = asVector (mapG f)
    zipG f = asVector2 (zipG f)
    toComplexG = toComplexM
    fromComplexG = fromComplexM
    conjG = asVector conjV
    asComplexG m@(M r c _) = toComplexG (m, reshape c (constant 0 (r*c) ))
    addG = addM
    scaleG = scaleM
    pnormG = pnormM

gmap :: (Field a, Field b, Container c) => (a -> b) -> c a -> c b
gmap = mapG

gzip :: (Field a, Field b, Field c, Container k) => (a -> b -> c) -> k a -> k b -> k c
gzip = zipG

-- | obtains the complex conjugate of a complex vector
conjV :: CVector -> CVector
conjV v = asComplex $ flatten $ reshape 2 (asReal v) `multiplyR` diag (fromList [1,-1])


conj :: Container t => t (Complex Double) -> t (Complex Double)
conj = conjG

toComplex :: Container t => (t Double, t Double) ->  t (Complex Double)
toComplex = toComplexG
fromComplex :: Container t => t (Complex Double) -> (t Double, t Double)
fromComplex = fromComplexG


-- | transpose of a matrix
trans :: Field t => Matrix t -> Matrix t
trans = transM

-- | extraction of a submatrix from a matrix
subMatrix :: Field t
           => (Int,Int) -- ^ (r0,c0) starting position
           -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
           -> Matrix t -> Matrix t
subMatrix = subMatrixM

-- | Creates a matrix from a list of vectors, as columns
fromColumns :: Field t => [Vector t] -> Matrix t
fromColumns m = trans . fromRows $ m

-- | Creates a list of vectors from the columns of a matrix
toColumns :: Field t => Matrix t -> [Vector t]
toColumns m = toRows . trans $ m

-- | creates a matrix from a vertical list of matrices
joinVert :: Field t => [Matrix t] -> Matrix t
joinVert ms = case common cols ms of
    Nothing -> error "joinVert on matrices with different number of columns"
    Just c  -> reshape c $ join (map flatten ms)

-- | creates a matrix from a horizontal list of matrices
joinHoriz :: Field t => [Matrix t] -> Matrix t
joinHoriz ms = trans. joinVert . map trans $ ms

{- | Creates a matrix from blocks given as a list of lists of matrices:

@\> let a = 'diag' $ 'fromList' [5,7,2]
\> let b = 'reshape' 4 $ 'constant' (-1) 12
\> dispR 2 $ fromBlocks [[a,b],[b,a]]
matrix (6x7)
 5. |  0. |  0. | -1. | -1. | -1. | -1.
 0. |  7. |  0. | -1. | -1. | -1. | -1.
 0. |  0. |  2. | -1. | -1. | -1. | -1.
-1. | -1. | -1. | -1. |  5. |  0. |  0.
-1. | -1. | -1. | -1. |  0. |  7. |  0.
-1. | -1. | -1. | -1. |  0. |  0. |  2.@

-}
fromBlocks :: Field t => [[Matrix t]] -> Matrix t
fromBlocks = joinVert . map joinHoriz 

-- | creates a complex vector from vectors with real and imaginary parts
toComplexV :: (Vector Double, Vector Double) ->  Vector (Complex Double)
toComplexV (r,i) = asComplex $ flatten $ fromColumns [r,i]

-- | extracts the real and imaginary parts of a complex vector
fromComplexV :: Vector (Complex Double) -> (Vector Double, Vector Double)
fromComplexV m = (a,b) where [a,b] = toColumns $ reshape 2 $ asReal m

-- | creates a complex matrix from matrices with real and imaginary parts
toComplexM :: (Matrix Double, Matrix Double) ->  Matrix (Complex Double)
toComplexM (r,i) = reshape (cols r) $ asComplex $ flatten $ fromColumns [flatten r, flatten i]

-- | extracts the real and imaginary parts of a complex matrix
fromComplexM :: Matrix (Complex Double) -> (Matrix Double, Matrix Double)
fromComplexM m = (reshape c a, reshape c b)
    where c = cols m
          [a,b] = toColumns $ reshape 2 $ asReal $ flatten m 

asRow :: Field a => Vector a -> Matrix a
asRow v = reshape (size v) v

asColumn :: Field a => Vector a -> Matrix a
asColumn v = reshape 1 v

complex :: Container t => t Double -> t (Complex Double)
complex = asComplexG

------------------------------------------------------------------

fromStorableArray1D :: Storable t => StorableArray Int t -> IO (Vector t)
fromStorableArray1D arr = do
    (l,u) <- getBounds arr
    let n = u-l+1
    p <- mallocForeignPtrArray n
    withStorableArray arr $ \parr ->
        withForeignPtr p $ \p ->
            copyArray p parr n
    return (V n p)

toStorableArray1D :: Storable t => Vector t -> IO(StorableArray Int t)
toStorableArray1D (V n p) = do
    arr <- newArray_ (0, n-1) 
    withForeignPtr p $ \p ->
        withStorableArray arr $ \parr -> copyArray parr p n
    return arr

-- | Creates a 'Vector' from an ordinary Haskell 'Array' @(@'Int'@)@. (It is transformed into a 'StorableArray' and then efficiently copied using 'withStorableArray' and 'copyArray'.)
fromArray1D :: Field t => Array Int t -> Vector t
fromArray1D arr = unsafePerformIO $ thaw arr >>= fromStorableArray1D

-- | The inverse of 'fromArray1D'.
toArray1D :: Field t => Vector t -> Array Int t
toArray1D v = unsafePerformIO $ toStorableArray1D v >>= freeze

-------------------------------------------------------------------------

instance (Field a, Show a) => Show (Vector a) where
    show v@(V n _) = "vector ("++show n++") "++show (toList v)

-------------------------------------------------------------------------

{- | Creates a real vector containing a range of values:

> > linspace 10 (-2,2)
>-2. -1.556 -1.111 -0.667 -0.222 0.222 0.667 1.111 1.556 2.

-}
linspace :: Int -> (Double, Double) -> Vector Double
linspace n (a,b) = fromList [a::Double,a+delta .. b]
    where delta = (b-a)/(fromIntegral n -1)

--------------------------------------------------------------------------

-- | Reads a vector position.
(@>) :: Field t => Vector t -> Int -> t
infixl 9 @>
(@>) = at

-- | Reads a matrix position.
(@@>) :: Field t => Matrix t -> (Int,Int) -> t
infixl 9 @@> 
(M r c p) @@> (i,j)
    | i<0 || i>=r || j<0 || j>=c = error "matrix indexing out of range"
    | otherwise   = unsafePerformIO $ do
                        withForeignPtr p $ \p ->
                            peek (advancePtr p (i*c+j)) 

----------------------------------------------------------------------------

instance (Field a, Show a) => Show (Matrix a) where
    show m@(M r c _) = "matrix ("++show r++"x"++show c++") "++show (toList $ flatten m)

-----------------------------------------------------------------------------

fromStorableArray2D :: Storable t => StorableArray (Int,Int) t -> IO (Matrix t)
fromStorableArray2D arr = do
    ((r1,c1),(r2,c2)) <- getBounds arr
    let r = r2-r1+1
    let c = c2-c1+1
    p <- mallocForeignPtrArray (r*c)
    withStorableArray arr $ \parr ->
        withForeignPtr p $ \p ->
            copyArray p parr (r*c)
    return (M r c p)

toStorableArray2D :: Storable t => Matrix t -> IO(StorableArray (Int,Int) t)
toStorableArray2D (M r c p) = do
    arr <- newArray_ ((0,0),(r-1,c-1)) 
    withForeignPtr p $ \p ->
        withStorableArray arr $ \ptr -> copyArray ptr p (r*c)
    return arr

-- | Creates a 'Matrix' from an ordinary Haskell 'Array' @(@'Int'@,@'Int'@)@. (It is transformed into a 'StorableArray' and then efficiently copied using 'withStorableArray' and 'copyArray'.)
fromArray2D :: Field t => Array (Int,Int) t -> Matrix t
fromArray2D arr = unsafePerformIO $ thaw arr >>= fromStorableArray2D

-- | The inverse of 'fromArray2D'.
toArray2D :: Field t => Matrix t -> Array (Int,Int) t
toArray2D m = unsafePerformIO $ toStorableArray2D m >>= freeze

---------------------------------------------------------------

-- | Creates a 'Matrix' from a list of lists (considered as rows).
fromLists :: Field t => [[t]] -> Matrix t
fromLists = fromRows . map fromList

-- | The inverse of 'fromLists'
toLists :: Field t => Matrix t -> [[t]]
toLists = map toList . toRows

----------------------------------------------------------------

-- | Creates a matrix with the first n rows of another matrix
takeRows :: Field t => Int -> Matrix t -> Matrix t
takeRows n mat = subMatrix (0,0) (n, cols mat) mat
-- | Creates a copy of a matrix without the first n rows
dropRows :: Field t => Int -> Matrix t -> Matrix t
dropRows n mat = subMatrix (n,0) (rows mat - n, cols mat) mat
-- |Creates a matrix with the first n columns of another matrix
takeColumns :: Field t => Int -> Matrix t -> Matrix t
takeColumns n mat = subMatrix (0,0) (rows mat, n) mat
-- | Creates a copy of a matrix without the first n columns
dropColumns :: Field t => Int -> Matrix t -> Matrix t
dropColumns n mat = subMatrix (0,n) (rows mat, cols mat - n) mat

-----------------------------------------------------------------

{- | The identity matrix of order N.

> > ident 4
> 1. 0. 0. 0.
> 0. 1. 0. 0.
> 0. 0. 1. 0.
> 0. 0. 0. 1.

-}
ident :: Int -> Matrix Double
ident = diag . constant 1

-------------------------------------------------------------------

-- | matrix product
mXm :: Field t => Matrix t -> Matrix t -> Matrix t
mXm = multiplyM


-- | euclidean inner product
dot :: Field t => Vector t -> Vector t -> t
dot u v = (asRow u `mXm` asColumn v) @@>(0,0)

-- | matrix - vector product
mXv :: Field t => Matrix t -> Vector t -> Vector t
mXv m v = flatten $ m `mXm` (asColumn v)

-- | vector - matrix product
vXm :: Field t => Vector t -> Matrix t -> Vector t
vXm v m = flatten $ (asRow v) `mXm` m

offsetC :: Complex Double -> CVector -> CVector
offsetC x = fromList . map (+x). toList

norm2 :: RVector -> Double
norm2 = toScalar 1 

norm1 :: RVector -> Double
norm1 = toScalar 2 

vectorMax :: Vector Double -> Double
vectorMax = toScalar 4
vectorMin :: Vector Double -> Double
vectorMin = toScalar 6
vectorMaxIndex :: Vector Double -> Int
vectorMaxIndex = round . toScalar 3
vectorMinIndex :: Vector Double -> Int
vectorMinIndex = round . toScalar 5

-------------------------------------------------------------------

add :: (Container c, Field t) => c t -> c t -> c t
add = addG

scale :: (Container c, Field t) => t -> c t -> c t
scale = scaleG

----------------------------------------------------------------------------

vmap = gmap

pnormRV 2 = norm2
pnormRV 1 = norm1
pnormRV 0 = vectorMax . vectorMap 3
pnormRV _ = error "p norm not yet defined"

pnormCV 2 = norm2 . asReal
pnormCV 1 = norm1 . vmap magnitude
pnormCV 0 = vectorMax . vmap magnitude
pnormCV _ = error "p norm not yet defined"

pnormRM 2 m = head (toList s) where (_,s,_) = svdg m
pnormRM 1 m = vectorMax $ constant 1 (rows m) `vXm` asVector (vectorMap 3) m
pnormRM 0 m = vectorMax $ asVector (vectorMap 3) m `mXv` constant 1 (cols m)
pnormRM _ _ = error "p norm not yet defined"

pnormCM 2 m = maxvalsing m
  where maxvalsing m = sqrt . abs . head . toList . fst . eigHg $ mm
        mm = if (rows m) > (cols m) then (conj.trans) m `mXm` m
                                    else m `mXm` (conj.trans) m
pnormCM 1 m = vectorMax $ constant 1 (rows m) `vXm` asVector (vmap magnitude) m
pnormCM 0 m = vectorMax $ asVector (vmap magnitude) m `mXv` constant 1 (cols m)
pnormCM _ _ = error "p norm not yet defined"

-- | computes the p-norm of a matrix or vector (with the same definitions as GNU-octave). pnorm 0 denotes \\inf-norm. See also 'norm'.
pnorm :: (Container t, Field a) => Int -> t a -> Double
pnorm = pnormG

------------------------------------------------------------------------------

extract l is = [l!!i |i<-is]

{- auxiliary function to get triangular matrices
-}
triang r c h v = reshape c $ fromList [el i j | i<-[0..r-1], j<-[0..c-1]]
    where el i j = if j-i>=h then v else (1::Double) - v
    
{- | rearranges the rows of a matrix according to the order given in a list of integers. 

> > extractRows [3,3,0,1] (ident 4)
> 0. 0. 0. 1.
> 0. 0. 0. 1.
> 1. 0. 0. 0.
> 0. 1. 0. 0.

-}
extractRows :: Field t => [Int] -> Matrix t -> Matrix t
extractRows l m = fromRows $ extract (toRows $ m) l

------------------------------------------------------------------------------

-- | Reverse rows 
flipud :: Field t => Matrix t -> Matrix t
flipud m = fromRows . reverse . toRows $ m

-- | Reverse columns
fliprl :: Field t => Matrix t -> Matrix t
fliprl m = fromColumns . reverse . toColumns $ m

-------------------------------------------------------------------------------


class Joinable a b c | a b -> c where
    joinH :: a -> b -> c
    joinV :: a -> b -> c

instance Joinable RMatrix RVector RMatrix where
    joinH m v = fromBlocks [[m,reshape 1 v]]
    joinV m v = fromBlocks [[m],[reshape (size v) v]]

instance Joinable RVector RMatrix RMatrix where
    joinH v m = fromBlocks [[reshape 1 v,m]]
    joinV v m = fromBlocks [[reshape (size v) v],[m]]

instance Joinable RMatrix RMatrix RMatrix where
    joinH m1 m2 = fromBlocks [[m1,m2]]
    joinV m1 m2 = fromBlocks [[m1],[m2]]

instance Joinable CMatrix CVector CMatrix where
    joinH m v = fromBlocks [[m,reshape 1 v]]
    joinV m v = fromBlocks [[m],[reshape (size v) v]]

instance Joinable CVector CMatrix CMatrix where
    joinH v m = fromBlocks [[reshape 1 v,m]]
    joinV v m = fromBlocks [[reshape (size v) v],[m]]

instance Joinable CMatrix CMatrix CMatrix where
    joinH m1 m2 = fromBlocks [[m1,m2]]
    joinV m1 m2 = fromBlocks [[m1],[m2]]

infixl 3 <|>, <->

{- | Horizontal concatenation of matrices and vectors:

@\> 'ident' 3 \<-\> i\<\>'ident' 3 \<|\> 'fromList' [1..6]
 1.   0.   0.  1.
 0.   1.   0.  2.
 0.   0.   1.  3.
1.i   0.   0.  4.
 0.  1.i   0.  5.
 0.   0.  1.i  6.@
-}
(<|>) :: (Joinable a b c) => a -> b -> c
a <|> b = joinH a b

-- | Vertical concatenation of matrices and vectors.
(<->) :: (Joinable a b c) => a -> b -> c
a <-> b = joinV a b

{- | Machine precision of a Double.     

>> eps
> 2.22044604925031e-16

(The value used by GNU-Octave)

-}
eps :: Double
eps =  2.22044604925031e-16

{- | The imaginary unit

@> 'ident' 3 \<\> i
1.i   0.   0.
 0.  1.i   0.
 0.   0.  1.i@

-}
i :: Complex Double
i = 0:+1 


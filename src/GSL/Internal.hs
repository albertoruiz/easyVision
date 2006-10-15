{-# OPTIONS #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GSL.Internal
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
module GSL.Internal where

import Foreign
import Complex
import GSL.GSL

-- | Generic GSL vector (C array 1D)
data GSLVector t = V Int (ForeignPtr t)

-- | Number of elements of a vector.
size :: GSLVector t -> Int
size (V n _) = n

-- | Generic GSL matrix (C array 2D)
data GSLMatrix t = M Int Int (ForeignPtr t)

-- | Number of rows of a matrix.
rows :: GSLMatrix t -> Int
rows (M r _ _) = r
-- | Number of columns of a matrix.
cols :: GSLMatrix t -> Int
cols (M _ c _) = c

-- | creates a GSLVector taking a number of consecutive elements from another GSLVector
subVector :: (Storable t) =>   Int       -- ^ index of the starting element
                            -> Int       -- ^ number of elements to extract 
                            -> GSLVector t  -- ^ source
                            -> GSLVector t  -- ^ result
subVector k l (V n p)
    | k<0 || k >= n || k+l > n || l < 0 = error "subVector out of range"
    | otherwise = unsafePerformIO $ do
        q <- mallocForeignPtrArray l
        withForeignPtr p $ \p ->
            withForeignPtr q $ \ q ->
                copyArray q (advancePtr p k) l
        return (V l q)

-- | creates a new GSLVector by joining a list of Vectors
join :: (Storable t) => [GSLVector t] -> GSLVector t
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


----------------------------------------------------------------------
instance (Storable a, RealFloat a) => Storable (Complex a) where    --
    alignment x = alignment (realPart x)                            --
    sizeOf x    = 2 * sizeOf (realPart x)                           --
    peek p = do                                                     --
        [re,im] <- peekArray 2 (castPtr p)                          --
        return (re :+ im)                                           --
    poke p (a :+ b) = pokeArray (castPtr p) [a,b]                   --
----------------------------------------------------------------------

-- | transforms a complex vector into a real vector with alternating real and imaginary parts 
asReal :: GSLVector (Complex Double) -> GSLVector Double 
asReal (V n p) = V (2*n) (castForeignPtr p)

-- | transforms a real vector into a complex vector with alternating real and imaginary parts
asComplex :: GSLVector Double -> GSLVector (Complex Double)
asComplex (V n p) = V (n `quot` 2) (castForeignPtr p)

-- | transpose of real matrix
transR :: GSLMatrix Double -> GSLMatrix Double
transR x@(M r c p) = unsafePerformIO $ do
    q <- mallocForeignPtrArray (r*c)
    withForeignPtr p $ \p ->
        withForeignPtr q $ \q ->
            prot "transR" $ c_transR r c p c r q
    return (M c r q)

{- | Creates a matrix from a vector by grouping the elements in rows with the desired number of columns.

@\> reshape 4 ('GSL.Interface.realVector' [1..12])
1.  2.  3.  4.
5.  6.  7.  8.
9. 10. 11. 12.@

-}
reshape :: Int -> GSLVector t -> GSLMatrix t
reshape c (V n p) | n `rem` c /= 0 = error "reshape"
                  | otherwise = M r c p where r = n `quot` c

{- | Creates a vector by concatenation of rows

@\> flatten ('GSL.Derived.ident' 3)
1. 0. 0. 0. 1. 0. 0. 0. 1.@
-}
flatten :: GSLMatrix t -> GSLVector t
flatten (M r c p) = V (r*c) p

-- | adapts a function on vectors to work on all the elements of matrices
asVector :: (GSLVector a -> GSLVector b) -> GSLMatrix a -> GSLMatrix b
asVector f m = reshape (cols m) . f . flatten $ m

sameShape (M r1 c1 _) (M r2 c2 _) = r1==r2 && c1 == c2 

-- | adapts a function on two vectors to work on all the elements of two matrices
asVector2 :: (GSLVector a -> GSLVector b -> GSLVector c) -> GSLMatrix a -> GSLMatrix b -> GSLMatrix c
asVector2 f m1 m2 
    | sameShape m1 m2 = reshape (cols m1) $ f (flatten m1) (flatten m2)
    | otherwise = error "inconsistent matrix dimensions" 

-- | creates a GSLMatrix from a list of vectors
fromRows :: (Storable t) => [GSLVector t] -> GSLMatrix t
fromRows vs = case common size vs of
    Nothing -> error "fromRows applied to [] or to vectors with different sizes"
    Just c  -> reshape c (join vs)

-- | extracts the rows of a matrix as a list of vectors
toRows :: (Storable t) => GSLMatrix t -> [GSLVector t]
toRows x = toRows' 0 where
    v = flatten x
    r = rows x
    c = cols x
    toRows' k | k == r*c  = []
              | otherwise = subVector k c v : toRows' (k+c)

-- | Creates a matrix from a list of vectors, as columns
fromColumnsR :: [GSLVector Double] -> GSLMatrix Double
fromColumnsR m = transR . fromRows $ m

-- | Creates a list of vectors from the columns of a matrix
toColumnsR :: GSLMatrix Double -> [GSLVector Double]
toColumnsR m = toRows . transR $ m

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


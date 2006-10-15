{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSL.Vector
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Basic operations on vectors.

-}
-----------------------------------------------------------------------------
module GSL.Vector (
    GSLVector,
    size,
    fromList, toList,
    fromArray, toArray,
    (!:),
    subVector, join,
    complex, parts
) where

import GSL.GSL
import Foreign
import Data.Complex
import Data.Array.Storable
import Data.Array(Array)

-- | Generic GSL vector (C array 1D)
data GSLVector t = V Int (ForeignPtr t)

-- | Number of elements of a vector.
size :: GSLVector t -> Int
size (V n _) = n

-- | Creates a vector from a list.  
fromList :: (Storable a) => [a] -> GSLVector a
fromList [] = error "trying to create an empty GSL vector"
fromList l = unsafePerformIO $ do
    let n = length l
    p <- mallocForeignPtrArray n
    withForeignPtr p $ \p ->
        pokeArray p l
    return (V n p)

-- | Creates a list from a vector.
toList :: (Storable t) => GSLVector t -> [t]
toList (V n p) = unsafePerformIO $ withForeignPtr p $ peekArray n

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


-- | Reading a vector position
(!:) :: (Storable t) => GSLVector t -> Int -> t
infixl 9 !:
(V n p) !: k
    | k<0 || k>=n = error "vector indexing out of range"
    | otherwise   = unsafePerformIO $ do
                        withForeignPtr p $ \p ->
                            peek (advancePtr p k) 


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

{- | Creates a vector from a standard Haskell @StorableArray@ indexed by @(Int)@:

>import GSL
>import Data.Array.Storable
>
>main = do 
>    hv <- newArray (1,10) 37 
>    print (bounds hv)
>    els <- getElems hv
>    print els
>    v <- fromStorableArrayV hv :: IO (ComplexVector)
>    print v
>    print (norm v)
>
>> main
>(1,10)
>[37.0 :+ 0.0,37.0 :+ 0.0,37.0 :+ 0.0, (etc.),37.0 :+ 0.0]
>37.  37.  37.  37.  37.  37.  37.  37.  37.  37.
>
>117.00427342623003

The elements are efficiently copied using @withStorableArray@ and @copyArray@.

-}
fromStorableArray :: Storable t => StorableArray Int t -> IO (GSLVector t)
fromStorableArray arr = do
    (l,u) <- getBounds arr
    let n = u-l+1
    p <- mallocForeignPtrArray n
    withStorableArray arr $ \parr ->
        withForeignPtr p $ \p ->
            copyArray p parr n
    return (V n p)


{- | Creates a @StorableArray@ indexed by @(Int)@ from a GSLVector. The elements are efficiently copied using @withStorableArray@ and @copyArray@.

-}
toStorableArray :: Storable t => GSLVector t -> IO(StorableArray Int t)
toStorableArray (V n p) = do
    arr <- newArray_ (0, n-1) 
    withForeignPtr p $ \p ->
        withStorableArray arr $ \parr -> copyArray parr p n
    return arr

fromArray :: Storable t => Array Int t -> GSLVector t
fromArray arr = unsafePerformIO $ thaw arr >>= fromStorableArray

toArray :: Storable t => GSLVector t -> Array Int t
toArray v = unsafePerformIO $ toStorableArray v >>= freeze

-------------------------------------------------------------------------
instance (Storable a, Show a) => Show (GSLVector a) where
    show v@(V n _) = "vector ("++show n++") "++show (toList v)

-------------------------------------------------------------------------

complex :: (GSLVector Double, GSLVector Double) -> GSLVector (Complex Double)
complex (r,i)
    | size r /= size i = error "trying to build a complex vector from parts with different sizes"
    | otherwise = unsafePerformIO $ do
        let (V n p) = join [r,i]
        let m = n `quot` 2
        q <- mallocForeignPtrArray n
        withForeignPtr p $ \p ->
            withForeignPtr q $ \q ->
                prot "complex" $ c_transR 2 m p m 2 q
        return $ asComplex $ V n q

parts :: GSLVector (Complex Double) -> (GSLVector Double, GSLVector Double) 
parts c = unsafePerformIO $ do
    let (V n p) = asReal c
    let m = n `quot` 2
    q <- mallocForeignPtrArray n
    withForeignPtr p $ \p ->
        withForeignPtr q $ \q ->
            prot "parts" $ c_transR m 2 p 2 m q
    let r = V n q
    return (subVector 0 m r, subVector m m r)


-- | transforms a complex vector into a real vector with alternating real and imaginary parts 
asReal :: GSLVector (Complex Double) -> GSLVector Double 
asReal (V n p) = V (2*n) (castForeignPtr p)

-- | transforms a real vector into a complex vector with alternating real and imaginary parts
asComplex :: GSLVector Double -> GSLVector (Complex Double)
asComplex (V n p) = V (n `quot` 2) (castForeignPtr p)

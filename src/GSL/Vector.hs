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
    size, (!:),
    fromList, toList,
    fromArray, toArray,
    subVector, join,
    fromComplex, toComplex,
    constant, linspace
) where

import GSL.GSL
import GSL.Internal
import Foreign
import Data.Complex
import Data.Array.Storable
import Data.Array(Array)


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



-- | Reading a vector position
(!:) :: (Storable t) => GSLVector t -> Int -> t
infixl 9 !:
(V n p) !: k
    | k<0 || k>=n = error "vector indexing out of range"
    | otherwise   = unsafePerformIO $ do
                        withForeignPtr p $ \p ->
                            peek (advancePtr p k) 



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

-- | creates a complex vector from vectors with real and imaginary parts
toComplex :: (GSLVector Double, GSLVector Double) ->  GSLVector (Complex Double)
toComplex (r,i) = asComplex $ flatten $ fromColumnsR [r,i]

-- | extracts the real and imaginary parts of a complex vector
fromComplex :: GSLVector (Complex Double) -> (GSLVector Double, GSLVector Double)
fromComplex m = (a,b) where [a,b] = toColumnsR $ reshape 2 $ asReal m



constant :: Double -> Int -> GSLVector Double
constant v n = unsafePerformIO $ do
    p <- mallocForeignPtrArray n
    withForeignPtr p $ \p ->
        prot "constant" $ c_constant v n p
    return (V n p)

{- | Creates a real vector containing a range of values:

> > linspace 10 (-2,2)
>-2. -1.556 -1.111 -0.667 -0.222 0.222 0.667 1.111 1.556 2.

-}
linspace :: Int -> (Double, Double) -> GSLVector Double
linspace n (a,b) = fromList [a::Double,a+delta .. b]
    where delta = (b-a)/(fromIntegral n -1)

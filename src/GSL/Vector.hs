{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSL.Vector
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

An interface to the gsl vectors (C arrays)

-}
-----------------------------------------------------------------------------
module GSL.Vector (
    Vector
) where

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
fromList l = createV [] "fromList1" (length l) $
    \n p -> do pokeArray p l
               return 0

-- | Creates a list from a vector.
toList1 :: (Storable t) => GSLVector t -> [t]
toList1 (V n p) = unsafePerformIO $ withForeignPtr p $ peekArray n

-- | creates a GSLVector taking a number of consecutive elements from another GSLVector
subVector :: (Storable t) =>   Int       -- ^ index of the starting element
                            -> Int       -- ^ number of elements to extract 
                            -> GSLVector t  -- ^ source
                            -> GSLVector t  -- ^ result
subVector k l x@(V n pt) 
    | k<0 || k >= n || k+l > n || l < 0 = error "subVector out of range"         
    | otherwise = createV [pt] "subVector" l $ v f x 
 where f n p l q = do copyArray q (advancePtr p k) l 
                      return 0


-- | Reading a vector position.         
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
join as = createV pas "join" tot (joiner as) where
    tot = sum (map size as)
    joiner [] _ _ = return 0
    joiner (V n b : cs) _ p = do
        withForeignPtr b  $ \b' -> copyArray p b' n
        joiner cs 0 (advancePtr p n)  -- the length of the result is not used in joiner
    pas = map (\(V _ p) -> p) as

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
fromStorableArrayV :: Storable t => StorableArray Int t -> IO (GSLVector t) 
fromStorableArrayV arr = do
    (l,u) <- getBounds arr
    let n = u-l+1
    let f n p = do 
        withStorableArray arr $ \ptr -> copyArray p ptr n
        return 0
    touchStorableArray arr
    return $ createV [] "fromStorableArrayV" n f

{- | Creates a @StorableArray@ indexed by @(Int)@ from a GSLVector. The elements are efficiently copied using @withStorableArray@ and @copyArray@.

-}
toStorableArrayV :: Storable t => GSLVector t -> IO(StorableArray Int t)
toStorableArrayV (V n p) = do
    arr <- newArray_ (0, n-1) 
    withForeignPtr p $ \p ->
        withStorableArray arr $ \ptr -> copyArray ptr p n
    return arr


fromArrayV :: Storable t => Array Int t -> GSLVector t
fromArrayV arr = unsafePerformIO $ do
    sa <- thaw arr
    v <- fromStorableArrayV sa
    return v

toArrayV :: Storable t => GSLVector t -> Array Int t
toArrayV v = unsafePerformIO $ do
    sa <- toStorableArrayV v
    arr <- freeze sa
    return arr

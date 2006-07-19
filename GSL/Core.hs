{-# OPTIONS -O -fffi #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GSL.Core
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL-style
-- 
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- Basic infrastructure to access the GSL (<http://www.gnu.org/software/gsl>).
--
-----------------------------------------------------------------------------

module GSL.Core  where

import Foreign
import Data.Complex
import Data.Array.Storable
import Data.Array(Array)


----------------------------------------------------------------------
instance (Storable a, RealFloat a) => Storable (Complex a) where    --
    alignment x = alignment (realPart x)                            --   
    sizeOf x    = 2 * sizeOf (realPart x)                           --
    peek p = do                                                     --
        [re,im] <- peekArray 2 (castPtr p)                          --
        return (re :+ im)                                           --
    poke p (a :+ b) = pokeArray (castPtr p) [a,b]                   --
----------------------------------------------------------------------

-- | Generic GSL vector (C array 1D)
data GSLVector t = V Int (ForeignPtr t)
-- | Generic GSL matrix (C array 2D)
data GSLMatrix t = M Int Int (ForeignPtr t)

-- | Vector of real (double precision) numbers.
type Vector = GSLVector Double
-- | Matrix with real (double precision) components.
type Matrix = GSLMatrix Double 
-- | Vector of complex (double precision) numbers.
type ComplexVector = GSLVector (Complex Double)
-- | Matrix with complex (double precision) components.
type ComplexMatrix = GSLMatrix (Complex Double)

-- | Number of elements of a vector.
size :: GSLVector t -> Int
size (V n _) = n
-- | Number of rows of a matrix.
rows :: GSLMatrix t -> Int
rows (M r _ _) = r
-- | Number of columns of a matrix.
cols :: GSLMatrix t -> Int
cols (M _ c _) = c

-- | Creates a vector from a list.  
fromList1 :: (Storable a) => [a] -> GSLVector a
fromList1 [] = error "trying to create an empty GSL vector"
fromList1 l = createV "fromList1" (length l) $
    \n p -> do pokeArray p l
               return 0

-- | Creates a list from a vector.
toList1 :: (Storable t) => GSLVector t -> [t]
toList1 (V n p) = unsafePerformIO $ withForeignPtr p $ peekArray n

{- | Creates a matrix from a vector by grouping the elements in rows with the desired number of columns.

@\> reshape 4 ('GSL.Interface.realVector' [1..12])
1.  2.  3.  4.
5.  6.  7.  8.
9. 10. 11. 12.@

-}
reshape :: Int -> GSLVector t -> GSLMatrix t
reshape c (V n p) = M r c p where r = n `quot` c

{- | Creates a vector by concatenation of rows

@\> flatten ('GSL.Derived.ident' 3)
1. 0. 0. 0. 1. 0. 0. 0. 1.@
-}
flatten :: GSLMatrix t -> GSLVector t
flatten (M r c p) = V (r*c) p


-- | obtains the common value of a property of a list
common :: (Eq a) => (b->a) -> [b] -> Maybe a
common f = commonval . map f where
    commonval :: (Eq a) => [a] -> Maybe a
    commonval [] = Nothing
    commonval [a] = Just a
    commonval (a:b:xs) = if a==b then commonval (b:xs) else Nothing

-- | creates a GSLMatrix from a list of vectors
fromRows :: (Storable t) => [GSLVector t] -> GSLMatrix t
fromRows vs = case common size vs of
    Nothing -> error "fromRows on vectors with different sizes" 
    Just c  -> reshape c (join vs)

-- | extracts the rows of a matrix as a list of vectors
toRows :: (Storable t) => GSLMatrix t -> [GSLVector t]
toRows x = toRows' 0 where
    v = flatten x
    r = rows x
    c = cols x
    toRows' k | k == r*c  = []
              | otherwise = subVector k c v : toRows' (k+c)
              
-- | adapts a function on vectors to work on all the elements of matrices
asVector :: (GSLVector a -> GSLVector b) -> GSLMatrix a -> GSLMatrix b
asVector f m = reshape (cols m) . f . flatten $ m
 
sameShape (M r1 c1 _) (M r2 c2 _) = r1==r2 && c1 == c2 
 
-- | adapts a function on two vectors to work on all the elements of two matrices
asVector2 :: (GSLVector a -> GSLVector b -> GSLVector c) -> GSLMatrix a -> GSLMatrix b -> GSLMatrix c
asVector2 f m1 m2 
    | sameShape m1 m2 = reshape (cols m1) $ f (flatten m1) (flatten m2)
    | otherwise = error "inconsistent matrix dimensions" 
 
-- | transforms a complex vector into a real vector with alternating real and imaginary parts 
asReal :: ComplexVector -> Vector 
asReal (V n p) = V (2*n) (castForeignPtr p)

-- | transforms a real vector into a complex vector with alternating real and imaginary parts
asComplex :: Vector -> ComplexVector
asComplex (V n p) = V (n `quot` 2) (castForeignPtr p)

-----------------------------------------------------------------------------
-- | creates a GSLVector taking a number of consecutive elements from another GSLVector
subVector :: (Storable t) =>   Int       -- ^ index of the starting element
                            -> Int       -- ^ number of elements to extract 
                            -> GSLVector t  -- ^ source
                            -> GSLVector t  -- ^ result
subVector k l x@(V n _) 
    | k<0 || k >= n || k+l > n || l < 0 = error "subVector out of range"         
    | otherwise = createV "subVector" l $ v f x 
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
    
-- | Reading a matrix position.         
(!!:) :: (Storable t) => GSLMatrix t -> (Int,Int) -> t
infixl 9 !!: 
(M r c p) !!: (i,j) 
    | i<0 || i>=r || j<0 || j>=c = error "matrix indexing out of range"
    | otherwise   = unsafePerformIO $ do
                        withForeignPtr p $ \p ->
                            peek (advancePtr p (i*c+j)) 

--------------------------------------------------------
-- | creates a new GSLVector by joining a list of Vectors
join :: (Storable t) => [GSLVector t] -> GSLVector t
join [] = error "joining an empty list"
join as = createV "join" tot (joiner as) where
    tot = sum (map size as)
    joiner [] _ _ = return 0
    joiner (V n b : cs) _ p = do
        withForeignPtr b  $ \b' -> copyArray p b' n
        joiner cs 0 (advancePtr p n)  -- the length of the result is not used in joiner
            
---------------------------------------------------------------------
------ creation of one or several vectors/matrices using a function f
---------------------------------------------------------------------
createV s n f = unsafePerformIO $ do
    p <- mallocForeignPtrArray n
    prot s $ withForeignPtr p (f n)
    return (V n p)
                 
createM s r c f = unsafePerformIO $ do
    p <- mallocForeignPtrArray (r*c)
    prot s $ withForeignPtr p (f r c)
    return (M r c p)

createVM s n r c f = unsafePerformIO $ do
    p <- mallocForeignPtrArray n
    q <- mallocForeignPtrArray (r*c)
    prot s $
     withForeignPtr p $ \p -> 
      withForeignPtr q $ \q -> 
       f n p r c q
    return (V n p, M r c q)
    
createMM s r1 c1 r2 c2 f = unsafePerformIO $ do
    p <- mallocForeignPtrArray (r1*c1)
    q <- mallocForeignPtrArray (r2*c2)
    prot s $
     withForeignPtr p $ \p -> 
      withForeignPtr q $ \q -> 
       f r1 c1 p r2 c2 q
    return (M r1 c1 p, M r2 c2 q)    

createMVM t r1 c1 n r2 c2 f = unsafePerformIO $ do
    p <- mallocForeignPtrArray (r1*c1)
    s <- mallocForeignPtrArray n
    q <- mallocForeignPtrArray (r2*c2)
    prot t $
     withForeignPtr p $ \p -> 
      withForeignPtr s $ \s -> 
       withForeignPtr q $ \q -> 
        f r1 c1 p n s r2 c2 q
    return (M r1 c1 p, V n s, M r2 c2 q)
        
----------------------------------------------------------------        


---------------------------------------------------------
-------------- argument transformers --------------------
---------------------------------------------------------
v f (V n p) = unsafePerformIO $ do
    let p' = unsafeForeignPtrToPtr p 
    let res = f n p'
    touchForeignPtr p
    return res

m f (M r c p) = unsafePerformIO $ do
    let p' = unsafeForeignPtrToPtr p 
    let res = f r c p'
    touchForeignPtr p
    return res

vv f a = v ((v f) a)
mm f a = m ((m f) a)
vvv f a b = v ((v f a) b)
------------------------------------------------
---------- signatures of the C functions -------
------------------------------------------------
type PD = Ptr Double                          --
type PC = Ptr (Complex Double)                --
type TV = Int -> PD -> IO Int                 --
type TVV = Int -> PD -> TV                    --
type TVVV = Int -> PD -> TVV                  -- 
type TM = Int -> Int -> PD -> IO Int          -- 
type TMM =  Int -> Int -> PD -> TM            -- 
type TMMM =  Int -> Int -> PD -> TMM          -- 
type TVM = Int -> PD -> TM                    -- 
type TVVM = Int -> PD -> TVM                  --
type TMV = Int -> Int -> PD -> TV             -- 
type TMVM = Int -> Int -> PD -> TVM           -- 
type TMMVM = Int -> Int -> PD -> TMVM         --
type TCM = Int -> Int -> PC -> IO Int         --
type TCVCM = Int -> PC -> TCM                 -- 
type TCMCM = Int -> Int -> PC -> TCM          -- 
type TVCM = Int -> PD -> TCM                  --
type TCMVCM = Int -> Int -> PC -> TVCM        --
type TCMCMCM = Int -> Int -> PC -> TCMCM      -- 
type TCV = Int -> PC -> IO Int                -- 
type TCVCV = Int -> PC -> TCV                 --
type TCMCV = Int -> Int -> PC -> TCV          --
type TVCV = Int -> PD -> TCV                  -- 
------------------------------------------------

prucreateV n f = unsafePerformIO $ do
    p <- mallocForeignPtrArray n
    prot "pru" $ withForeignPtr p (f n) 
    return (V n p)


prot msg f = do
    errorcode <- f
    case errorcode of
        0    -> return ()
        1000 -> error $ "size problem in the GSL wrapper: " ++ msg
        1001 -> error $ "unknown opcode in the GSL wrapper: " ++ msg
        1002 -> error $ "memory allocation problem in GSL wrapper: " ++ msg
        1003 -> error $ "wrong file name in GSL wrapper: " ++ msg
    
-------------------------------------------------------------------
------------- fast creation from storable arrays ------------------

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
    let (l,u) = bounds arr
    let n = u-l+1
    let f n p = do 
        withStorableArray arr $ \ptr -> copyArray p ptr n
        return 0
    return $ createV "fromStorableArrayV" n f

{- | Creates a @StorableArray@ indexed by @(Int)@ from a GSLVector. The elements are efficiently copied using @withStorableArray@ and @copyArray@.

-}
toStorableArrayV :: Storable t => GSLVector t -> IO(StorableArray Int t)
toStorableArrayV (V n p) = do
    arr <- newArray_ (0, n-1) 
    withForeignPtr p $ \p ->
        withStorableArray arr $ \ptr -> copyArray ptr p n
    return arr

{- | Creates a matrix from a standard Haskell @StorableArray@ indexed by @(Int,Int)@. The elements are efficiently copied using @withStorableArray@ and @copyArray@.

-}
fromStorableArrayM :: Storable t => StorableArray (Int,Int) t -> IO (GSLMatrix t) 
fromStorableArrayM arr = do
    let ((r1,c1),(r2,c2)) = bounds arr
    let r = r2-r1+1
    let c = c2-c1+1
    let f r c p = do 
        withStorableArray arr $ \ptr -> copyArray p ptr (r*c)
        return 0
    return $ createM "fromStorableArrayM" r c f
    
{- | Creates @StorableArray@ indexed by @(Int,Int)@ from a matrix.

The elements are efficiently copied using @withStorableArray@ and @copyArray@.

-}
toStorableArrayM :: Storable t => GSLMatrix t -> IO(StorableArray (Int,Int) t) 
toStorableArrayM (M r c p) = do
    arr <- newArray_ ((0,0),(r-1,c-1)) 
    withForeignPtr p $ \p ->
        withStorableArray arr $ \ptr -> copyArray ptr p (r*c)
    return arr   
    
---------------------------------------------------------------    
    
fromArrayV :: Storable t => Array Int t -> GSLVector t
fromArrayV arr = unsafePerformIO $ do
    sa <- thaw arr
    v <- fromStorableArrayV sa
    return v
    
fromArrayM :: Storable t => Array (Int,Int) t -> GSLMatrix t
fromArrayM arr = unsafePerformIO $ do
    sa <- thaw arr
    m <- fromStorableArrayM sa
    return m
    
toArrayV :: Storable t => GSLVector t -> Array Int t
toArrayV v = unsafePerformIO $ do
    sa <- toStorableArrayV v
    arr <- freeze sa
    return arr
    
toArrayM :: Storable t => GSLMatrix t -> Array (Int,Int) t
toArrayM m = unsafePerformIO $ do
    sa <- toStorableArrayM m
    arr <- freeze sa
    return arr

--------------------------------------------------------

{- | conversion of Haskell functions into function pointers that can be used in the C side
-}
foreign import ccall "wrapper" mkfun:: (Double -> Ptr() -> Double) -> IO( FunPtr (Double -> Ptr() -> Double)) 


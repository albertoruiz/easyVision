{-# OPTIONS -fffi #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GSL.Base
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL-style
-- 
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- Basic interface to the GSL (<http://www.gnu.org/software/gsl>).
--
-----------------------------------------------------------------------------

module GSL.Base  where

import Foreign
import Complex

----------------------------------------------------------------------
instance (Storable a, RealFloat a) => Storable (Complex a) where    --
    alignment x = alignment (realPart x)                            --   
    sizeOf x    = 2 * sizeOf (realPart x)                           --
    peek p = do                                                     --
        [re,im] <- peekArray 2 (castPtr p)                          --
        return (re :+ im)                                           --
    poke p (a :+ b) = pokeArray (castPtr p) [a,b]                   --
----------------------------------------------------------------------

-- | generic GSL vector
data Vector t = V Int (ForeignPtr t)
-- | generic GSL matrix
data Matrix t = M Int Int (ForeignPtr t)

type V = Vector Double
type M = Matrix Double 
type CV = Vector (Complex Double)
type CM = Matrix (Complex Double)

-- | number of elements of a vector
size :: Vector t -> Int
size (V n _) = n
-- | number of rows of a matrix
rows :: Matrix t -> Int
rows (M r _ _) = r
-- | number of columns of a matrix
cols :: Matrix t -> Int
cols (M _ c _) = c

-- | Creates a vector from a list. Related functions: 'realVector', 'complexVector', 'fromLists', and 'flatten'. 
fromList :: (Storable a) => [a] -> Vector a
fromList [] = error "trying to create an empty GSL vector"
fromList l = createV "fromList" (length l) $
    \n p -> do pokeArray p l
               return 0

-- | creates a list from a vector.
toList :: (Storable t) => Vector t -> [t]
toList (V n p) = unsafePerformIO $ withForeignPtr p $ peekArray n

{- | creates a matrix from a vector by grouping the elements in rows with the desired number of columns.

> > reshape 4 $ fromList [1..12] :: M
> 1.  2.  3.  4.
> 5.  6.  7.  8.
> 9. 10. 11. 12.

-}
reshape :: Int -> Vector t -> Matrix t
reshape c (V n p) = M r c p where r = n `quot` c

-- | creates a vector by concatenation of rows
flatten :: Matrix t -> Vector t
flatten (M r c p) = V (r*c) p


-- | obtains the common value of a property of a list
common :: (Eq a) => (b->a) -> [b] -> Maybe a
common f = commonval . map f where
    commonval :: (Eq a) => [a] -> Maybe a
    commonval [] = Nothing
    commonval [a] = Just a
    commonval (a:b:xs) = if a==b then commonval (b:xs) else Nothing

-- | creates a Matrix from a list of vectors
fromRows :: (Storable t) => [Vector t] -> Matrix t
fromRows vs = case common size vs of
    Nothing -> error "fromRows on vectors with different sizes" 
    Just c  -> reshape c (join vs)

-- | extracts the rows of a matrix as a list of vectors
toRows :: (Storable t) => Matrix t -> [Vector t]
toRows x = toRows' 0 where
    v = flatten x
    r = rows x
    c = cols x
    toRows' k | k == r*c  = []
              | otherwise = subVector k c v : toRows' (k+c)
              
-- | adapts a function on vectors to work on all the elements of matrices
asVector :: (Vector a -> Vector b) -> Matrix a -> Matrix b
asVector f m = reshape (cols m) . f . flatten $ m
 
sameShape (M r1 c1 _) (M r2 c2 _) = r1==r2 && c1 == c2 
 
-- | adapts a function on two vectors to work on all the elements of two matrices
asVector2 :: (Vector a -> Vector b -> Vector c) -> Matrix a -> Matrix b -> Matrix c
asVector2 f m1 m2 
    | sameShape m1 m2 = reshape (cols m1) $ f (flatten m1) (flatten m2)
    | otherwise = error "inconsistent matrix dimensions" 
 
-- | transforms a complex vector into a real vector with alternating real and imaginary parts 
asReal :: CV -> V 
asReal (V n p) = V (2*n) (castForeignPtr p)

-- | transforms a real vector into a complex vector with alternating real and imaginary parts
asComplex :: V -> CV
asComplex (V n p) = V (n `quot` 2) (castForeignPtr p)

-----------------------------------------------------------------------------
-- | creates a Vector taking a number of consecutive elements from another Vector
subVector :: (Storable t) =>   Int       -- ^ index of the starting element
                            -> Int       -- ^ number of elements to extract 
                            -> Vector t  -- ^ source
                            -> Vector t  -- ^ result
subVector k l x@(V n _) 
    | k<0 || k >= n || k+l > n || l < 0 = error "subVector out of range"         
    | otherwise = createV "subVector" l $ v f x 
 where f n p l q = do copyArray q (advancePtr p k) l 
                      return 0


-- | Reading a vector position.         
(!:) :: (Storable t) => Vector t -> Int -> t
infixl 9 !: 
(V n p) !: k 
    | k<0 || k>=n = error "vector indexing out of range"
    | otherwise   = unsafePerformIO $ do
                        withForeignPtr p $ \p ->
                            peek (advancePtr p k) 
    
-- | Reading a matrix position.         
(!!:) :: (Storable t) => Matrix t -> (Int,Int) -> t
infixl 9 !!: 
(M r c p) !!: (i,j) 
    | i<0 || i>=r || j<0 || j>=c = error "matrix indexing out of range"
    | otherwise   = unsafePerformIO $ do
                        withForeignPtr p $ \p ->
                            peek (advancePtr p (i*c+j)) 

--------------------------------------------------------
-- | creates a new Vector by joining a list of Vectors
join :: (Storable t) => [Vector t] -> Vector t
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
    
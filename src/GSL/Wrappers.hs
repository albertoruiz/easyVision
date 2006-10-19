{-# OPTIONS #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LinearAlgebra.Wrappers
-- Copyright   :  (c) Alberto Ruiz 2006
-- License     :  GPL-style
-- 
-- Maintainer  :  Alberto Ruiz (aruiz at um dot es)
-- Stability   :  provisional
-- Portability :  portable (uses FFI)
--
-- Wrappers for the GSL functions (<http://www.gnu.org/software/gsl>).
--
-----------------------------------------------------------------------------

-- #hide

module GSL.Wrappers where

import GSL.Types
import Foreign
import Foreign.C.Types
import Foreign.C.String

-- | RVector of real (double precision) numbers.
type RVector = Vector Double
-- | RMatrix with real (double precision) components.
type RMatrix = Matrix Double 
-- | RVector of complex (double precision) numbers.
type CVector = Vector (Complex Double)
-- | RMatrix with complex (double precision) components.
type CMatrix = Matrix (Complex Double)


-- | transforms a complex vector into a real vector with alternating real and imaginary parts 
asReal :: Vector (Complex Double) -> Vector Double 
asReal (V n p) = V (2*n) (castForeignPtr p)

-- | transforms a real vector into a complex vector with alternating real and imaginary parts
asComplex :: Vector Double -> Vector (Complex Double)
asComplex (V n p) = V (n `quot` 2) (castForeignPtr p)

{- | Creates a matrix from a vector by grouping the elements in rows with the desired number of columns.

@\> reshape 4 ('GSL.Interface.realVector' [1..12])
1.  2.  3.  4.
5.  6.  7.  8.
9. 10. 11. 12.@

-}
reshape :: Int -> Vector t -> Matrix t
reshape c (V n p) | n `rem` c /= 0 = error "reshape"
                  | otherwise = M r c p where r = n `quot` c

{- | Creates a vector by concatenation of rows

@\> flatten ('GSL.Derived.ident' 3)
1. 0. 0. 0. 1. 0. 0. 0. 1.@
-}
flatten :: Matrix t -> Vector t
flatten (M r c p) = V (r*c) p

-- | Reads a vector position
(@>) :: (Storable t) => Vector t -> Int -> t
infixl 9 @>
(V n p) @> k
    | k<0 || k>=n = error "vector indexing out of range"
    | otherwise   = unsafePerformIO $ do
                        withForeignPtr p $ \p ->
                            peek (advancePtr p k)

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

prot :: String -> IO Int -> IO ()
prot msg f = do
    errorcode <- f
    case errorcode of
        0    -> return ()
        1000 -> error $ "size problem in the GSL wrapper: " ++ msg
        1001 -> error $ "unknown opcode in the GSL wrapper: " ++ msg
        1002 -> error $ "memory allocation problem in GSL wrapper: " ++ msg
        1003 -> error $ "wrong file name in GSL wrapper: " ++ msg
        n    -> error $ "unknown code " ++ (show n)


---------- DEPRECATED --------------------------------

createV ps s n f = unsafePerformIO $ do
    p <- mallocForeignPtrArray n
    prot s $ withForeignPtr p (f n)
    mapM_ touchForeignPtr ps
    return (V n p)

createM ps s r c f = unsafePerformIO $ do
    p <- mallocForeignPtrArray (r*c)
    prot s $ withForeignPtr p (f r c)
    mapM_ touchForeignPtr ps
    return (M r c p)

createVM ps s n r c f = unsafePerformIO $ do
    p <- mallocForeignPtrArray n
    q <- mallocForeignPtrArray (r*c)
    prot s $
     withForeignPtr p $ \p ->
      withForeignPtr q $ \q ->
       f n p r c q
    mapM_ touchForeignPtr ps
    return (V n p, M r c q)

createMM ps s r1 c1 r2 c2 f = unsafePerformIO $ do
    p <- mallocForeignPtrArray (r1*c1)
    q <- mallocForeignPtrArray (r2*c2)
    prot s $
     withForeignPtr p $ \p ->
      withForeignPtr q $ \q ->
       f r1 c1 p r2 c2 q
    mapM_ touchForeignPtr ps
    return (M r1 c1 p, M r2 c2 q)

createMVM ps t r1 c1 n r2 c2 f = unsafePerformIO $ do
    p <- mallocForeignPtrArray (r1*c1)
    s <- mallocForeignPtrArray n
    q <- mallocForeignPtrArray (r2*c2)
    prot t $
     withForeignPtr p $ \p ->
      withForeignPtr s $ \s ->
       withForeignPtr q $ \q ->
        f r1 c1 p n s r2 c2 q
    mapM_ touchForeignPtr ps
    return (M r1 c1 p, V n s, M r2 c2 q)

---------------------------------------------------------
-------------- argument transformers --------------------
---------------------------------------------------------
-- the foreign pointers must be touched by the above create functions.
-- (the previous approach didn't work)

v f (V n p) = f n (unsafeForeignPtrToPtr p)

m f (M r c p) = f r c (unsafeForeignPtrToPtr p)

vv f a b = v (v f a) b
mm f a b = m (m f a) b
vvv f a b c = v (v f a b) c

-------------- WRAPPERS ---------------------------------

-- | creates a constant vector
constant :: Double -> Int -> Vector Double
constant v n = unsafePerformIO $ do
    p <- mallocForeignPtrArray n
    withForeignPtr p $ \p ->
        prot "constant" $ c_constant v n p
    return (V n p)
foreign import ccall "gslaux.h constant" c_constant :: Double -> TV

-- | diagonal matrix from a real vector
diagR :: RVector -> RMatrix
diagR x@(V n p) = createM [p] "diagR" n n $ v c_diagR x
foreign import ccall "gslaux.h diagR" c_diagR :: TVM

-- | diagonal matrix from a real vector
diagC :: CVector -> CMatrix
diagC x@(V n p) = createM [p] "diagC" n n $ v c_diagC x
foreign import ccall "gslaux.h diagC" c_diagC :: TCVCM

-- | extracts the diagonal of a real matrix
takeDiagR :: RMatrix -> RVector
takeDiagR x@(M r c p) = createV [p] "take_diagonal" (min r c) $ m c_take_diagonal x
foreign import ccall "gslaux.h take_diagonal" c_take_diagonal :: TMV

-- | extracts the diagonal of a complex matrix
takeDiagC :: CMatrix -> CVector
takeDiagC x@(M r c p) = createV [p] "take_diagonalC" (min r c) $ m c_take_diagonalC x
foreign import ccall "gslaux.h take_diagonalC" c_take_diagonalC :: TCMCV

-- | transpose of real matrix
transR :: Matrix Double -> Matrix Double
transR x@(M r c p) = unsafePerformIO $ do
    q <- mallocForeignPtrArray (r*c)
    withForeignPtr p $ \p ->
        withForeignPtr q $ \q ->
            prot "transR" $ c_transR r c p c r q
    return (M c r q)
foreign import ccall "gslaux.h trans" c_transR :: TMM

-- | transpose of complex matrix
transC :: Matrix (Complex Double) -> Matrix (Complex Double)
transC x@(M r c p) = unsafePerformIO $ do
    q <- mallocForeignPtrArray (r*c)
    withForeignPtr p $ \p ->
        withForeignPtr q $ \q ->
            prot "transC" $ c_transC r c p c r q
    return (M c r q)
foreign import ccall "gslaux.h transC" c_transC :: TCMCM

-- | extraction of a submatrix of a real matrix
subMatrixR :: (Int,Int) -- ^ (r0,c0) starting position 
           -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
           -> Matrix Double -> Matrix Double
subMatrixR (r0,c0) (rt,ct) x@(M r c p) = unsafePerformIO $ do
    q <- mallocForeignPtrArray (r*c)
    withForeignPtr p $ \p ->
        withForeignPtr q $ \q ->
            prot "subMatrixR" $ c_submatrixR r0 (r0+rt-1) c0 (c0+ct-1) r c p rt ct q
    return (M rt ct q)
foreign import ccall "gslaux.h submatrixR" c_submatrixR :: Int -> Int -> Int -> Int -> TMM

-- | extraction of a submatrix of a complex matrix
subMatrixC :: (Int,Int) -- ^ (r0,c0) starting position
           -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
           -> CMatrix -> CMatrix
subMatrixC (r0,c0) (rt,ct) x@(M r c _) = 
    reshape ct . asComplex . flatten .
    subMatrixR (r0,2*c0) (rt,2*ct) .
    reshape (2*c) . asReal . flatten $ x

-- | scaling of a real vector
scaleR :: Double -> RVector -> RVector
scaleR a x@(V n p) = createV [p] "scaleR" n $ v (c_scaleR a) x
foreign import ccall "gslaux.h vector_scaleR" c_scaleR :: Double -> TVV

-- | scaling of a real vector
scaleC :: Complex Double -> CVector -> CVector
scaleC (a:+b) x@(V n p) = createV [p] "scaleC" n $ v (c_scaleC a b) x
foreign import ccall "gslaux.h vector_scaleC" c_scaleC :: Double -> Double -> TCVCV

-- | add constant to a real vector
addConstant :: Double -> RVector -> RVector
addConstant a x@(V n p) = createV [p] "vector_offset" n $ v (c_vectorOffset a) x
foreign import ccall "gslaux.h vector_offset" c_vectorOffset :: Double -> TVV

-- | obtains different functions of a vector: norm1, norm2, max, min, posmax, posmin, etc.
toScalar :: Int -> RVector -> Double
toScalar code x@(V n p) =  (createV [p] "toScalar" 1 $ v (c_toScalar code) x) @> 0
foreign import ccall "gslaux.h toScalar" c_toScalar :: Int -> TVV

-- | Mapeo de vectores con una función deseada
vectorMap :: Int -> RVector -> RVector
vectorMap code x@(V n p) = createV [p] "vectorMap" n $ v (c_vectorMap code) x
foreign import ccall "gslaux.h vectorMap" c_vectorMap :: Int -> TVV 

-- | elementwise operation on vectors
vectorZip :: Int -> RVector -> RVector -> RVector
vectorZip code x@(V n p) y@(V _ q) = createV [p,q] "vectorZip" n $ vv (c_vectorZip code) x y
foreign import ccall "gslaux.h vectorZip" c_vectorZip :: Int -> TVVV


{- | eigendecomposition of a real symmetric matrix using /gsl_eigen_symmv/.

> > let (l,v) = eigS $ fromList2 [[1,2],[2,1]]
> > l
> 3.000 -1.000
>
> > v
> 0.707 -0.707
> 0.707  0.707
>
> > v <> diag l <> trans v
> 1.000 2.000
> 2.000 1.000

-}
eigS :: RMatrix -> (RVector, RMatrix)
eigS x@(M r c p) = unsafePerformIO $ do
    l <- mallocForeignPtrArray r
    v <- mallocForeignPtrArray (r*r)
    withForeignPtr p $ \pp ->
        withForeignPtr l $ \pl ->
            withForeignPtr v $ \pv ->
                prot "eigS" $ c_eigS r c pp r pl r r pv
    return (V r l, M r r v)
foreign import ccall "gslaux.h eigensystemR" c_eigS :: TMVM

------------------------------------------------------------------



{- | eigendecomposition of a complex hermitian matrix using /gsl_eigen_hermv/

> > let (l,v) = eigH $ fromList2 [[1,2+i],[2-i,3]]
>
> > l
> 4.449 -0.449
>
> > v
>         -0.544          0.839
> (-0.751,0.375) (-0.487,0.243)
>
> > v <> diag l <> (conj.trans) v
>          1.000 (2.000,1.000)
> (2.000,-1.000)         3.000

-}
eigH :: CMatrix -> (RVector, CMatrix)
eigH x@(M r c p) = unsafePerformIO $ do
    l <- mallocForeignPtrArray r
    v <- mallocForeignPtrArray (r*r)
    withForeignPtr p $ \pp ->
        withForeignPtr l $ \pl ->
            withForeignPtr v $ \pv ->
                prot "eigH" $ c_eigH r c pp r pl r r pv
    return (V r l, M r r v)
foreign import ccall "gslaux.h eigensystemC" c_eigH :: TCMVCM


{- | Singular value decomposition of a real matrix, using /gsl_linalg_SV_decomp_mod/:

@\> let (u,s,v) = svd $ 'GSL.Interface.realMatrix' [[1,2,3],[-4,1,7]]
\ 
\> u
0.310 -0.951
0.951  0.310
\  
\> s
8.497 2.792
\ 
\> v
-0.411 -0.785
 0.185 -0.570
 0.893 -0.243
\  
\> u \<\> 'GSL.Interface.diag' s \<\> 'GSL.Derived.trans' v
 1. 2. 3.
-4. 1. 7.@

-}
svd :: RMatrix -> (RMatrix, RVector, RMatrix)
svd' x@(M r c p) = createMVM [p] "svd" r c c c c $ m c_svd x
foreign import ccall "gslaux.h svd" c_svd :: TMMVM
svd x@(M r c _) = if r>=c 
    then svd' x
    else (v, s, u) where (u,s,v) = svd' (transR x)

{- | QR decomposition of a real matrix using /gsl_linalg_QR_decomp/ and /gsl_linalg_QR_unpack/.

@\> let (q,r) = qr $ 'GSL.Interface.realMatrix' [[1,3,5,7],[2,0,-2,4]]
\ 
\> q
-0.447 -0.894
-0.894  0.447
\ 
\> r
-2.236 -1.342 -0.447 -6.708
    0. -2.683 -5.367 -4.472
\ 
\> q \<\> r
1.000 3.000  5.000 7.000
2.000    0. -2.000 4.000@

-}
qr :: RMatrix -> (RMatrix, RMatrix)
qr x@(M r c p) = createMM [p] "QR" r r r c $ m c_qr x
foreign import ccall "gslaux.h QR" c_qr :: TMMM

{- | Cholesky decomposition of a symmetric positive definite real matrix using /gsl_linalg_cholesky_decomp/.

@\> let c = chol $ 'GSL.Interface.realMatrix' [[5,4],[4,5]]
\ 
\> c
2.236    0.
1.789 1.342
\ 
\> c \<\> 'GSL.Derived.trans' c
5.000 4.000
4.000 5.000@

-}
chol :: RMatrix -> RMatrix
chol x@(M r _ p) = createM [p] "chol" r r $ m c_chol x
foreign import ccall "gslaux.h chol" c_chol :: TMM


-- | real matrix product using /gsl_blas_dgemm/
multiplyR :: RMatrix -> RMatrix -> RMatrix
multiplyR x@(M r1 c1 p) y@(M r2 c2 q) =
  unsafePerformIO $ do
    r <- mallocForeignPtrArray (r1*c2)
    withForeignPtr p $ \p ->
        withForeignPtr q $ \q ->
            withForeignPtr r $ \r->
                prot "multiplyR" $ c_multiplyR r1 c1 p r2 c2 q r1 c2 r
    return (M r1 c2 r)
foreign import ccall "gslaux.h multiplyR" c_multiplyR :: TMMM


-- | complex matrix product /using gsl_blas_zgemm/
multiplyC :: CMatrix -> CMatrix -> CMatrix
multiplyC x@(M r _ p) y@(M _ c q) = createM [p,q] "multiplyC" r c $ mm c_multiplyC x y
foreign import ccall "gslaux.h multiplyC" c_multiplyC :: TCMCMCM

--------------------------------------------------------

{- | efficient multiplication by the inverse of a matrix (for real matrices). 
-}
luSolveR :: RMatrix -> RMatrix -> RMatrix
luSolveR a@(M r1 c1 p) b@(M r2 c2 q) = createM [p,q] "luSolveR" r1 c2 $ mm c_luSolveR a b
foreign import ccall "gslaux.h luSolveR" c_luSolveR ::  TMMM

{- | efficient multiplication by the inverse of a matrix (for complex matrices). 
-}
luSolveC :: CMatrix -> CMatrix -> CMatrix
luSolveC a@(M r1 c1 p) b@(M r2 c2 q) = createM [p,q] "luSolveC" r1 c2 $ mm c_luSolveC a b
foreign import ccall "gslaux.h luSolveC" c_luSolveC ::  TCMCMCM

{- | lu decomposition of real matrix (packed as a vector including l, u, the permutation and sign)
-}
luRaux  :: RMatrix -> RVector
luRaux a@(M r1 c1 p) = createV [p] "luR" (r1*r1+r1+1) $ m c_luRaux a
foreign import ccall "gslaux.h luRaux" c_luRaux :: TMV

{- | lu decomposition of complex matrix (packed as a vector including l, u, the permutation and sign)
-}
luCaux  :: CMatrix -> CVector
luCaux a@(M r1 c1 p) = createV [p] "luC" (r1*r1+r1+1) $ m c_luCaux a
foreign import ccall "gslaux.h luCaux" c_luCaux :: TCMCV

--------------------------------------------------------------

-- | loads a matrix efficiently from formatted ASCII text file (the number of rows and columns must be known in advance).
fromFile :: FilePath -> (Int,Int) -> IO (Matrix Double)
fromFile filename (r,c) = do
    charname <- newCString filename
    m <- mallocForeignPtrArray (r*c)
    withForeignPtr m $ \pm ->
        prot "gslReadMatrix" $ c_gslReadMatrix charname r c pm
    --free charname  -- TO DO: free the auxiliary CString
    return (M r c m)
foreign import ccall "gslaux.h matrix_fscanf" c_gslReadMatrix:: Ptr CChar -> TM

---------------------------------------------------------------------------

-- | experiment to send to opengl a mesh from C
meshC :: RMatrix -> IO Int
meshC x =  m c_mesh x
foreign import ccall "gslaux.h mesh" c_mesh :: Int -> Int -> Ptr Double -> IO Int

{- | conversion of Haskell functions into function pointers that can be used in the C side
-}
foreign import ccall "wrapper" mkfun:: (Double -> Ptr() -> Double) -> IO( FunPtr (Double -> Ptr() -> Double)) 

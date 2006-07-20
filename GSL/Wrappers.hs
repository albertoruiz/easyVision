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

module GSL.Wrappers where

import GSL.Core
import Foreign
import Foreign.C.Types

-- | creates a constant vector
constantV :: Double -> Int -> Vector
constantV val n = createV "constant" n (c_constant val)
foreign import ccall "gslaux.h constant" c_constant :: Double -> TV

-- | diagonal matrix from a real vector
diagR :: Vector -> Matrix
diagR x@(V n _) = createM "diagR" n n $ v c_diagR x
foreign import ccall "gslaux.h diagR" c_diagR :: TVM

-- | diagonal matrix from a real vector
diagC :: ComplexVector -> ComplexMatrix
diagC x@(V n _) = createM "diagC" n n $ v c_diagC x
foreign import ccall "gslaux.h diagC" c_diagC :: TCVCM

-- | extracts the diagonal of a real matrix
takeDiag :: Matrix -> Vector
takeDiag x@(M r c _) = createV "take_diagonal" (min r c) $ m c_take_diagonal x
foreign import ccall "gslaux.h take_diagonal" c_take_diagonal :: TMV

-- | extracts the diagonal of a complex matrix
takeDiagC :: ComplexMatrix -> ComplexVector
takeDiagC x@(M r c _) = createV "take_diagonalC" (min r c) $ m c_take_diagonalC x
foreign import ccall "gslaux.h take_diagonalC" c_take_diagonalC :: TCMCV

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
eigS :: Matrix -> (Vector, Matrix)
eigS x@(M r _ _) = createVM "eigensystemS" r r r $ m c_eigensystem x
foreign import ccall "gslaux.h eigensystemR" c_eigensystem :: TMVM

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
eigH :: ComplexMatrix -> (Vector, ComplexMatrix)
eigH x@(M r _ _) = createVM "eigensystemH" r r r $ m c_eigensystemC x
foreign import ccall "gslaux.h eigensystemC" c_eigensystemC :: TCMVCM


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
svd :: Matrix -> (Matrix, Vector, Matrix)
svd' x@(M r c _) = createMVM "svd" r c c c c $ m c_svd x
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
qr :: Matrix -> (Matrix, Matrix)
qr x@(M r c _) = createMM "QR" r r r c $ m c_qr x
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
chol :: Matrix -> Matrix
chol x@(M r _ _) = createM "chol" r r $ m c_chol x
foreign import ccall "gslaux.h chol" c_chol :: TMM


-- | real matrix product using /gsl_blas_dgemm/
multiply :: Matrix -> Matrix -> Matrix
multiply x@(M r _ _) y@(M _ c _) = createM "multiplyR" r c $ mm c_multiplyR x y
foreign import ccall "gslaux.h multiplyR" c_multiplyR :: TMMM

-- | complex matrix product /using gsl_blas_zgemm/
multiplyC :: ComplexMatrix -> ComplexMatrix -> ComplexMatrix
multiplyC x@(M r _ _) y@(M _ c _) = createM "multiplyC" r c $ mm c_multiplyC x y
foreign import ccall "gslaux.h multiplyC" c_multiplyC :: TCMCMCM

-- | transpose of real matrix
transR :: Matrix -> Matrix
transR x@(M r c _) = createM "transR" c r $ m c_trans x
foreign import ccall "gslaux.h trans" c_trans :: TMM

-- | transpose of real matrix
transC :: ComplexMatrix -> ComplexMatrix
transC x@(M r c _) = createM "transC" c r $ m c_transC x
foreign import ccall "gslaux.h transC" c_transC :: TCMCM

-- | extraction of a submatrix of a real matrix
subMatrixR :: (Int,Int) -- ^ (r0,c0) starting position 
           -> (Int,Int) -- ^ (rt,ct) dimensions of submatrix
           -> Matrix -> Matrix 
subMatrixR (r0,c0) (rt,ct) x@(M r c _) = createM "submatrixR" rt ct $ m (c_submatrixR r0 (r0+rt-1) c0 (c0+ct-1)) x
foreign import ccall "gslaux.h submatrixR" c_submatrixR :: Int -> Int -> Int -> Int -> TMM

-- | scaling of a real vector
scale :: Double -> Vector -> Vector
scale a x = createV "vector_scale" (size x) $ v (c_vectorScale a) x
foreign import ccall "gslaux.h vector_scale" c_vectorScale :: Double -> TVV
            
-- | add constant to a real vector
offset :: Double -> Vector -> Vector
offset a x = createV "vector_offset" (size x) $ v (c_vectorOffset a) x
foreign import ccall "gslaux.h vector_offset" c_vectorOffset :: Double -> TVV

-- | obtains different functions of a vector: norm1, norm2, max, min, posmax, posmin, etc.
toScalar :: Int -> Vector -> Double
toScalar code x =  (createV "toScalar" 1 $ v (c_toScalar code) x) !: 0
foreign import ccall "gslaux.h toScalar" c_toScalar :: Int -> TVV

-- | Mapeo de vectores con una función deseada
vectorMap :: Int -> Vector -> Vector
vectorMap code x = createV "vectorMap" (size x) $ v (c_vectorMap code) x 
foreign import ccall "gslaux.h vectorMap" c_vectorMap :: Int -> TVV 

-- | elementwise operation on vectors
vectorZip :: Int -> Vector -> Vector -> Vector
vectorZip code x y = createV "vectorZip" (size x) $ vv (c_vectorZip code) x y
foreign import ccall "gslaux.h vectorZip" c_vectorZip :: Int -> TVVV


--------------------------------------------------------

{- | efficient multiplication by the inverse of a matrix (for real matrices). 
-}
luSolveR :: Matrix -> Matrix -> Matrix
luSolveR a@(M r1 c1 _) b@(M r2 c2 _) = createM "luSolveR" r1 c2 $ mm c_luSolveR a b
foreign import ccall "gslaux.h luSolveR" c_luSolveR ::  TMMM
    
{- | efficient multiplication by the inverse of a matrix (for complex matrices). 
-}
luSolveC :: ComplexMatrix -> ComplexMatrix -> ComplexMatrix
luSolveC a@(M r1 c1 _) b@(M r2 c2 _) = createM "luSolveC" r1 c2 $ mm c_luSolveC a b
foreign import ccall "gslaux.h luSolveC" c_luSolveC ::  TCMCMCM
                     
{- | lu decomposition of real matrix (packed as a vector including l, u, the permutation and sign)
-}                     
luRaux  :: Matrix -> Vector
luRaux a@(M r1 c1 _) = createV "luR" (r1*r1+r1+1) $ m c_luRaux a
foreign import ccall "gslaux.h luRaux" c_luRaux :: TMV                   
                     
{- | lu decomposition of complex matrix (packed as a vector including l, u, the permutation and sign)
-}                     
luCaux  :: ComplexMatrix -> ComplexVector
luCaux a@(M r1 c1 _) = createV "luC" (r1*r1+r1+1) $ m c_luCaux a
foreign import ccall "gslaux.h luCaux" c_luCaux :: TCMCV                   

--------------------------------------------------------------

-- | loads a matrix efficiently from formatted ASCII text file (the number of rows and columns must be known in advance).
gslReadMatrix :: FilePath -> (Int,Int) -> IO Matrix
gslReadMatrix filename (r,c) = do
    charname <- newArray0 (toEnum . fromEnum $ 0) (map (toEnum.fromEnum) filename) 
    let m = createM "gslReadMatrix" r c $ c_gslReadMatrix charname
    --free charname  TO DO: free the auxiliary CString
    return m
foreign import ccall "gslaux.h matrix_fscanf" c_gslReadMatrix:: Ptr CChar -> TM

---------------------------------------------------------------------------


-----------------------------------------------------------------
-- | experiment to send to opengl a mesh from C
meshC :: Matrix -> IO Int
meshC x =  m c_mesh x
foreign import ccall "gslaux.h mesh" c_mesh :: Int -> Int -> Ptr Double -> IO Int

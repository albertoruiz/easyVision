{-# OPTIONS -fffi #-}

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
constantV :: Double -> Int -> V
constantV val n = createV "constant" n (c_constant val)
foreign import ccall "gslaux.h constant" c_constant :: Double -> TV

-- | diagonal matrix from a real vector
diagR :: V -> M
diagR x@(V n _) = createM "diagR" n n $ v c_diagR x
foreign import ccall "gslaux.h diagR" c_diagR :: TVM

-- | diagonal matrix from a real vector
diagC :: CV -> CM
diagC x@(V n _) = createM "diagC" n n $ v c_diagC x
foreign import ccall "gslaux.h diagC" c_diagC :: TCVCM

-- | extracts the diagonal of a real matrix
takeDiag :: M -> V
takeDiag x@(M r c _) = createV "take_diagonal" (min r c) $ m c_take_diagonal x
foreign import ccall "gslaux.h take_diagonal" c_take_diagonal :: TMV

-- | extracts the diagonal of a complex matrix
takeDiagC :: CM -> CV
takeDiagC x@(M r c _) = createV "take_diagonalC" (min r c) $ m c_take_diagonalC x
foreign import ccall "gslaux.h take_diagonalC" c_take_diagonalC :: TCMCV

{- | eigendecomposition of a real symmetric matrix using /gsl_eigen_symmv/.

> > let (l,v) = eigS $ fromLists [[1,2],[2,1]]
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
eigS :: M -> (V, M)
eigS x@(M r _ _) = createVM "eigensystemS" r r r $ m c_eigensystem x
foreign import ccall "gslaux.h eigensystemR" c_eigensystem :: TMVM

{- | eigendecomposition of a complex hermitian matrix using /gsl_eigen_hermv/

> > let (l,v) = eigH $ fromLists [[1,2+i],[2-i,3]]
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
eigH :: CM -> (V, CM)
eigH x@(M r _ _) = createVM "eigensystemH" r r r $ m c_eigensystemC x
foreign import ccall "gslaux.h eigensystemC" c_eigensystemC :: TCMVCM


{- | Singular value decomposition of a real matrix, using /gsl_linalg_SV_decomp_mod/:

> > let (u,s,v) = svd $ realMatrix [[1,2,3],[-4,1,7]]
>
> > u
> 0.310 -0.951
> 0.951  0.310
> 
> > s
> 8.497 2.792
>
> > v
> -0.411 -0.785
>  0.185 -0.570
>  0.893 -0.243
> 
> > u <> diag s <> trans v
>  1. 2. 3.
> -4. 1. 7.
-}
svd :: M -> (M, V, M)
svd' x@(M r c _) = createMVM "svd" r c c c c $ m c_svd x
foreign import ccall "gslaux.h svd" c_svd :: TMMVM
svd x@(M r c _) = if r>=c 
    then svd' x
    else (v, s, u) where (u,s,v) = svd' (transR x)

{- | QR decomposition of a real matrix using /gsl_linalg_QR_decomp/ and /gsl_linalg_QR_unpack/.

> > let (q,r) = qr $ realMatrix [[1,3,5,7],[2,0,-2,4]]
>
> > q
> -0.447 -0.894
> -0.894  0.447
>
> > r
> -2.236 -1.342 -0.447 -6.708
>     0. -2.683 -5.367 -4.472
>
> > q <> r
> 1.000 3.000  5.000 7.000
> 2.000    0. -2.000 4.000

-}
qr :: M -> (M, M)
qr x@(M r c _) = createMM "QR" r r r c $ m c_qr x
foreign import ccall "gslaux.h QR" c_qr :: TMMM

{- | Cholesky decomposition of a symmetric positive definite real matrix using /gsl_linalg_cholesky_decomp/.

> > let c = chol $ realMatrix [[5,4],[4,5]]
>
> > c
> 2.236    0.
> 1.789 1.342
>
> > c <> trans c
> 5.000 4.000
> 4.000 5.000

-}
chol :: M -> M
chol x@(M r _ _) = createM "chol" r r $ m c_chol x
foreign import ccall "gslaux.h chol" c_chol :: TMM


-- | real matrix product using /gsl_blas_dgemm/
multiply :: M -> M -> M
multiply x@(M r _ _) y@(M _ c _) = createM "multiplyR" r c $ mm c_multiplyR x y
foreign import ccall "gslaux.h multiplyR" c_multiplyR :: TMMM

-- | complex matrix product /using gsl_blas_zgemm/
multiplyC :: CM -> CM -> CM
multiplyC x@(M r _ _) y@(M _ c _) = createM "multiplyC" r c $ mm c_multiplyC x y
foreign import ccall "gslaux.h multiplyC" c_multiplyC :: TCMCMCM

-- | transpose of real matrix
transR :: M -> M
transR x@(M r c _) = createM "transR" c r $ m c_trans x
foreign import ccall "gslaux.h trans" c_trans :: TMM

-- | transpose of real matrix
transC :: CM -> CM
transC x@(M r c _) = createM "transC" c r $ m c_transC x
foreign import ccall "gslaux.h transC" c_transC :: TCMCM

-- | extraction of a submatrix of a real matrix
subMatrixR :: Int -- ^ r1 
          -> Int -- ^ r2
          -> Int -- ^ c1
          -> Int -- ^ c2
          -> M -> M 
subMatrixR r1 r2 c1 c2 x@(M r c _) = createM "submatrixR" (r2-r1+1) (c2-c1+1) $ m (c_submatrixR r1 r2 c1 c2) x
foreign import ccall "gslaux.h submatrixR" c_submatrixR :: Int -> Int -> Int -> Int -> TMM

-- | scaling of a real vector
scale :: Double -> V -> V
scale a x = createV "vector_scale" (size x) $ v (c_vectorScale a) x
foreign import ccall "gslaux.h vector_scale" c_vectorScale :: Double -> TVV
            
-- | add constant to a real vector
offset :: Double -> V -> V
offset a x = createV "vector_offset" (size x) $ v (c_vectorOffset a) x
foreign import ccall "gslaux.h vector_offset" c_vectorOffset :: Double -> TVV

-- | obtains different functions of a vector: norm1, norm2, max, min, posmax, posmin, etc.
toScalar :: Int -> V -> Double
toScalar code x =  (createV "toScalar" 1 $ v (c_toScalar code) x) !: 0
foreign import ccall "gslaux.h toScalar" c_toScalar :: Int -> TVV

-- | Mapeo de vectores con una función deseada
vectorMap :: Int -> V -> V
vectorMap code x = createV "vectorMap" (size x) $ v (c_vectorMap code) x 
foreign import ccall "gslaux.h vectorMap" c_vectorMap :: Int -> TVV 

-- | elementwise operation on vectors
vectorZip :: Int -> V -> V -> V
vectorZip code x y = createV "vectorZip" (size x) $ vv (c_vectorZip code) x y
foreign import ccall "gslaux.h vectorZip" c_vectorZip :: Int -> TVVV


genfft code x@(V n _) = createV "fft" n $ v (c_fft code) x
foreign import ccall "gslaux.h fft" c_fft ::  Int -> TCVCV
--fft  = genfft 0
--ifft = genfft 1

--------------------------------------------------------

{- | efficient multiplication by the inverse of a matrix (for real matrices). 
-}
luSolveR :: M -> M -> M
luSolveR a@(M r1 c1 _) b@(M r2 c2 _) = createM "luSolveR" r1 c2 $ mm c_luSolveR a b
foreign import ccall "gslaux.h luSolveR" c_luSolveR ::  TMMM
    
{- | efficient multiplication by the inverse of a matrix (for complex matrices). 
-}
luSolveC :: CM -> CM -> CM
luSolveC a@(M r1 c1 _) b@(M r2 c2 _) = createM "luSolveC" r1 c2 $ mm c_luSolveC a b
foreign import ccall "gslaux.h luSolveC" c_luSolveC ::  TCMCMCM
                     
{- | lu decomposition of real matrix (packed as a vector including l, u, the permutation and sign)
-}                     
luRaux  :: M -> V
luRaux a@(M r1 c1 _) = createV "luR" (r1*r1+r1+1) $ m c_luRaux a
foreign import ccall "gslaux.h luRaux" c_luRaux :: TMV                   
                     
{- | lu decomposition of complex matrix (packed as a vector including l, u, the permutation and sign)
-}                     
luCaux  :: CM -> CV
luCaux a@(M r1 c1 _) = createV "luC" (r1*r1+r1+1) $ m c_luCaux a
foreign import ccall "gslaux.h luCaux" c_luCaux :: TCMCV                   
                     
                     
--------------------------------------------------------

{- | conversion of Haskell functions into function pointers that can be used in the C side
-}
foreign import ccall "wrapper" mkfun:: (Double -> Ptr() -> Double) -> IO( FunPtr (Double -> Ptr() -> Double)) 
      
--------------------------------------------------------------------
{- | Numeric integration using /gsl_integration_qags/ (adaptive integration with singularities). For example:

> > let quad = integrateQAGS 1E-9 1000 
> > let f a x = x**(-0.5) * log (a*x)
> > quad (f 1) 0 1
> (-3.999999999999974,4.871658632055187e-13)
 
-}  

integrateQAGS :: Double               -- ^ precision (e.g. 1E-9)
                 -> Int               -- ^ size of auxiliary workspace (e.g. 1000)
                 -> (Double -> Double) -- ^ function to be integrated on the interval (a,b)
                 -> Double           -- ^ a
                 -> Double           -- ^ b
                 -> (Double, Double) -- ^ result of the integration and error
integrateQAGS prec n f a b = unsafePerformIO $ do
    r <- malloc
    e <- malloc
    fp <- mkfun (\x _ -> f x) 
    c_integrate_qags fp a b prec n r e
    vr <- peek r
    ve <- peek e
    let result = (vr,ve)
    free r
    free e
    freeHaskellFunPtr fp
    return result
                  
foreign import ccall "gslaux.h integrate_qags" 
 c_integrate_qags :: FunPtr (Double-> Ptr() -> Double) -> Double -> Double -> Double -> Int 
                     -> Ptr Double -> Ptr Double -> IO ()
                  
                       
-----------------------------------------------------------------

{- | Numeric integration using /gsl_integration_qng/ (useful for fast integration of smooth functions). For example:

> > let quad = integrateQNG 1E-6 
> > quad (\x -> 4/(1+x*x)) 0 1 
> (3.141592653589793,3.487868498008632e-14) 
 
-}  

integrateQNG :: Double               -- ^ precision (e.g. 1E-9)
                 -> (Double -> Double) -- ^ function to be integrated on the interval (a,b)
                 -> Double           -- ^ a
                 -> Double           -- ^ b
                 -> (Double, Double) -- ^ result of the integration and error
integrateQNG prec f a b = unsafePerformIO $ do
    r <- malloc
    e <- malloc
    fp <- mkfun (\x _ -> f x) 
    c_integrate_qng fp a b prec r e
    vr <- peek r
    ve <- peek e
    let result = (vr,ve)
    free r
    free e
    freeHaskellFunPtr fp
    return result
                  
foreign import ccall "gslaux.h integrate_qng" 
 c_integrate_qng :: FunPtr (Double-> Ptr() -> Double) -> Double -> Double -> Double 
                    -> Ptr Double -> Ptr Double -> IO ()

------------------------------------------------------------------------

{- | Solution of general polynomial equations, using /gsl_poly_complex_solve/. For example,
     the three solutions of x^3 + 8 = 0

> > polySolve $ realVector [8,0,0,1]
> -2.  1.+1.732i  1.-1.732i

The example in the GSL manual: To find the roots of x^5 -1 = 0:

> > toList $ polySolve (realVector [-1, 0, 0, 0, 0, 1]) 
> [(-0.8090169943749475) :+ 0.5877852522924731,
> (-0.8090169943749475) :+ (-0.5877852522924731),
> 0.30901699437494734 :+ 0.9510565162951536,
> 0.30901699437494734 :+ (-0.9510565162951536),
> 1.0 :+ 0.0]

-}  
polySolve :: V -> CV
polySolve x@(V n _) = createV "polySolve" (n-1) $ v c_polySolve x
foreign import ccall "gslaux.h polySolve" c_polySolve:: TVCV

------------------------------------------------------------------------

-- | loads a matrix efficiently from formatted ASCII text file (the number of rows and columns must be known in advance).
gslReadMatrix :: FilePath -> (Int,Int) -> IO M
gslReadMatrix filename (r,c) = do
    charname <- newArray0 (toEnum . fromEnum $ 0) (map (toEnum.fromEnum) filename) 
    let m = createM "gslReadMatrix" r c $ c_gslReadMatrix charname
    --free charname  TO DO: free the auxiliary CString
    return m
foreign import ccall "gslaux.h matrix_fscanf" c_gslReadMatrix:: Ptr CChar -> TM

---------------------------------------------------------------------------
{- | Minimization of a multidimensional function, using the method of Nelder and Mead, implemented by /gsl_multimin_fminimizer_nmsimplex/, and described in <http://www.gnu.org/software/gsl/manual/gsl-ref_35.html#SEC474>. The gradient of the function is not required.

@\> let minimize f xi = minimizeNMSimplex f xi (replicate (length xi) 1) 1e-6 100
\> let f [x,y] = (x-1)*(x-1) + (y+3)*(y+3)
\> minimize f [10,10]
([1.000000039316922,-3.0000005208092726],2.727881187741245e-13,66)@

-}
minimizeNMSimplex :: ([Double] -> Double) -- ^ function to minimize
          -> [Double]            -- ^ starting point
          -> [Double]            -- ^ sizes of the initial search box
          -> Double              -- ^ desired precision of the solution
          -> Int                 -- ^ maximum number of iterations allowed
          -> ([Double], Double, Int)   
          -- ^ solution vector, the value of the function at it, and the number of iterations performed by the algorithm      
minimizeNMSimplex f xi sz tol maxit = (sol,  val, round it) where
    val:it:sol = toList $ minimizeV (f.toList) tol maxit (fromList xi) (fromList sz)

minimizeV :: (V -> Double)       -- ^ function to minimize
          -> Double              -- ^ error tolerance
          -> Int                 -- ^ maximum number of iterations
          -> V                   -- ^ initial solution
          -> V                   -- ^ sizes of the search box
          -> V                   -- ^ solution and function value at it and iterations
          
minimizeV f tol maxit xi@(V n _) sz = unsafePerformIO $ do
    fp <- mkVecfun (iv f)
    let sol = createV "minimizeV" (n+2) $ vv (c_minimize fp tol maxit) xi sz
    --freeHaskellFunPtr fp
    return sol
foreign import ccall "gslaux.h minimize" 
 c_minimize:: FunPtr (Int -> Ptr Double -> Double) -> Double -> Int -> TVVV
    
iv :: (V -> Double) -> (Int -> Ptr Double -> Double)    
iv f n p = f (createV "iv" n copy) where
    copy n q = do 
        copyArray q p n
        return 0

-- | conversion of Haskell functions into function pointers that can be used in the C side
foreign import ccall "wrapper" mkVecfun:: (Int -> Ptr Double -> Double) -> IO( FunPtr (Int -> Ptr Double -> Double)) 
      
----------------------------------------------------------------
-------------------- simple functions --------------------------

{- | The error function (/gsl_sf_erf/), defined as 2\/ \\sqrt \\pi * \int\_0\^t \\exp -t\^2 dt.

@> erf 1.5
0.9661051464753108@

-}
foreign import ccall "gsl_sf_erf" erf :: Double -> Double

{- | The Gaussian probability density function (/gsl_sf_erf_Z/), defined as (1\/\\sqrt\{2\\pi\}) \\exp(-x\^2\/2).

>> erf_Z 1.5
>0.12951759566589172

-}
foreign import ccall "gsl_sf_erf_Z" erf_Z :: Double -> Double

-- | experiment to send to opengl a mesh from C
meshC :: M -> IO Int
meshC x =  m c_mesh x
foreign import ccall "gslaux.h mesh" c_mesh :: Int -> Int -> Ptr Double -> IO Int

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


--------------------------------------------------------------

{- | Numeric derivative using /gsl_deriv_central/. For example:
-}  
derivGen :: 
    Int                   -- ^ type: 0 central, 1 forward, 2 backward
    -> Double             -- ^ initial step size
    -> (Double -> Double) -- ^ function
    -> Double             -- ^ point where the derivative is taken
    -> (Double, Double)   -- ^ result and error
derivGen c h f x = unsafePerformIO $ do
    r <- malloc
    e <- malloc
    fp <- mkfun (\x _ -> f x) 
    c_deriv c fp x h r e
    vr <- peek r
    ve <- peek e
    let result = (vr,ve)
    free r
    free e
    freeHaskellFunPtr fp
    return result
                  
foreign import ccall "gslaux.h deriv" 
 c_deriv :: Int -> FunPtr (Double -> Ptr () -> Double) -> Double -> Double 
                    -> Ptr Double -> Ptr Double -> IO Int
{- | Adaptive central difference algorithm, /gsl_deriv_central/. For example:

> > let deriv = derivCentral 0.01 
> > deriv sin (pi/4)
>(0.7071067812000676,1.0600063101654055e-10)
> > cos (pi/4)
>0.7071067811865476 

-}
derivCentral :: Double                  -- ^ initial step size
                -> (Double -> Double)   -- ^ function 
                -> Double               -- ^ point where the derivative is taken
                -> (Double, Double)     -- ^ result and absolute error
derivCentral = derivGen 0
-- | Adaptive forward difference algorithm, /gsl_deriv_forward/. The function is evaluated only at points greater than x, and never at x itself. The derivative is returned in result and an estimate of its absolute error is returned in abserr. This function should be used if f(x) has a discontinuity at x, or is undefined for values less than x.
derivForward :: Double                  -- ^ initial step size
                -> (Double -> Double)   -- ^ function 
                -> Double               -- ^ point where the derivative is taken
                -> (Double, Double)     -- ^ result and absolute error
derivForward = derivGen 1
-- | Adaptive backward difference algorithm, /gsl_deriv_backward/. 
derivBackward ::Double                  -- ^ initial step size
                -> (Double -> Double)   -- ^ function 
                -> Double               -- ^ point where the derivative is taken
                -> (Double, Double)     -- ^ result and absolute error
derivBackward = derivGen 2

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

    
iv :: (V -> Double) -> (Int -> Ptr Double -> Double)    
iv f n p = f (createV "iv" n copy) where
    copy n q = do 
        copyArray q p n
        return 0

-- | conversion of Haskell functions into function pointers that can be used in the C side
foreign import ccall "wrapper" mkVecfun:: (Int -> Ptr Double -> Double) -> IO( FunPtr (Int -> Ptr Double -> Double)) 
            
-- | another required conversion 
foreign import ccall "wrapper" mkVecVecfun:: (Int -> Ptr Double -> Ptr Double -> IO ()) -> IO( FunPtr (Int -> Ptr Double -> Ptr Double->IO()))            
            
aux_vTov :: (V -> V) -> (Int -> Ptr Double -> Ptr Double -> IO())    
aux_vTov f n p r = g where
    (V _ pr) = f x
    x = createV "aux_vTov" n copy
    copy n q = do 
        copyArray q p n
        return 0
    g = withForeignPtr pr $ \pr -> copyArray r pr n
            
--------------------------------------------------------------------
-- | auxiliary function used by 'minimize'

minimizeV :: (V -> Double)       -- ^ function to minimize
          -> Double              -- ^ error tolerance
          -> Int                 -- ^ maximum number of iterations
          -> V                   -- ^ initial solution
          -> V                   -- ^ sizes of the search box
          -> M                   -- ^ matrix with solution, info and trajectory            
minimizeV f tol maxit xi@(V n _) sz = unsafePerformIO $ do
    fp <- mkVecfun (iv f)
    let sol = createM "minimizeVList" maxit (n+3) $ vv (c_minimizeList fp tol maxit) xi sz
    --freeHaskellFunPtr fp
    return sol
foreign import ccall "gslaux.h minimize" 
 c_minimizeList:: FunPtr (Int -> Ptr Double -> Double) -> Double -> Int -> TVVM
      
      
--------------------------------------------------------------      
      
minimizeDerivV :: (V -> Double)  -- ^ function to minimize
          -> (V -> V)            -- ^ gradient
          -> Double              -- ^ error tolerance
          -> Int                 -- ^ maximum number of iterations
          -> V                   -- ^ initial solution
          -> Double              -- ^ initial step size
          -> Double              -- ^ minimization parameter
          -> M                   -- ^ matrix with solution, info and trajectory            
minimizeDerivV f df tol maxit xi@(V n _) istep minimpar = unsafePerformIO $ do
    fp <- mkVecfun (iv f)
    dfp <- mkVecVecfun (aux_vTov df)
    let sol = createM "minimizeDerivV" maxit (n+2) $ 
                v (c_minimizeDeriv fp dfp istep minimpar tol maxit) xi
    --freeHaskellFunPtr fp
    return sol
foreign import ccall "gslaux.h minimizeWithDeriv" 
 c_minimizeDeriv:: FunPtr (Int -> Ptr Double -> Double) -> FunPtr (Int -> Ptr Double -> Ptr Double -> IO ())  -> Double -> Double -> Double -> Int -> TVM      
      
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

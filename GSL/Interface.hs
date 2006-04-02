{-# OPTIONS -fglasgow-exts #-}

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
-- High level and minimalist interface to the linear algebra functionality
--
-----------------------------------------------------------------------------

module GSL.Interface where

import GSL.Core
import GSL.Wrappers
import GSL.Derived
import Complex
import Data.List(transpose)
import Numeric(showGFloat)
import Foreign
import Data.Array.Storable (StorableArray)
import Data.Array


{- | The imaginary unit

@> 'ident' 3 \<\> i
1.i   0.   0.
 0.  1.i   0.
 0.   0.  1.i@

-}
i :: Complex Double
i = 0:+1 



class Mul a b c | a b -> c where
 infixl 7 <>
{- | Matrix product, matrix-vector product, dot product and scaling of vectors and matrices. Using this operator you can freely combine real and complex objects:

@v = 'realVector' [1,2,3]
cv = 'complexVector' [1+'i',2]
m = 'realMatrix' [[1,2,3],[4,5,7]]
cm = 'complexMatrix' [[1,2],[3+'i',7*'i'],['i',1]]
\ 
\> m \<\> v
14. 35.
\ 
\> cv \<\> m
9.+1.i  12.+2.i  17.+3.i
\ 
\> m \<\> cm
  7.+5.i   5.+14.i
19.+12.i  15.+35.i
\ 
\> v \<\> 'i'
1.i  2.i  3.i
\ 
\> v \<\> v
14.0
\ 
\> cv \<\> cv
4.0 :+ 2.0@

-}
 (<>) :: a -> b -> c         

class Add a b c | a b -> c where
 infixl 5 |+|
 -- | matrix and vector sum, add constant to vectors and matrices
 (|+|) :: a -> b -> c         

infixl 5 |-|
-- | matrix and vector difference, substract constant to vectors and matrices
(|-|) :: (Mul Double b d, Add a d c) => a -> b -> c
a |-| b = a |+| (-1::Double) <> b
 
 
instance Mul Double Double Double where
 (<>) = (*)
  
instance Mul Double (Complex Double) (Complex Double) where
 a <> b = (a:+0) * b
   
instance Mul (Complex Double) Double (Complex Double) where
 a <> b = a * (b:+0)
    
instance Mul (Complex Double) (Complex Double) (Complex Double) where
 (<>) = (*)    
   

----------------------------------------------

cv v = complexV (v, constant 0 v)
cm m = complexM (m, constant 0 m)

-- | map on vectors
vmap :: (Storable a, Storable b) => (a -> b) -> GSLVector a -> GSLVector b
vmap f = fromList1 . map f . toList1

-- | zipWith on vectors
vzip :: (Storable a, Storable b, Storable c) => (a -> b -> c) -> GSLVector a -> GSLVector b -> GSLVector c
vzip f a b = fromList1 $ zipWith f (toList1 a) (toList1 b)

-- | map on matrices
mmap :: (Storable a, Storable b) => (a -> b) -> GSLMatrix a -> GSLMatrix b
mmap m = asVector . vmap $ m

-- | zipWith on matrices
mzip :: (Storable a, Storable b, Storable c) => (a -> b -> c) -> GSLMatrix a -> GSLMatrix b -> GSLMatrix c
mzip f = asVector2 (vzip f)

--------------------------------- matrix matrix

instance Mul Matrix Matrix Matrix where
 (<>) = multiply

instance Mul ComplexMatrix ComplexMatrix ComplexMatrix where
 (<>) = multiplyC

instance Mul ComplexMatrix Matrix ComplexMatrix where
 c <> r = c <> cm r

instance Mul Matrix ComplexMatrix ComplexMatrix where
 r <> c = cm r <> c

--------------------------------- matrix vector

instance Mul Matrix Vector Vector where
 (<>) = mRvR

instance Mul ComplexMatrix ComplexVector ComplexVector where
 (<>) = mCvC

instance Mul ComplexMatrix Vector ComplexVector where
 m <> v = m <> cv v

instance Mul Matrix ComplexVector ComplexVector where
 m <> v = cm m <> v

--------------------------------- vector matrix

instance Mul Vector Matrix Vector where
 (<>) = vRmR
 
instance Mul ComplexVector ComplexMatrix ComplexVector where
 (<>) = vCmC
 
instance Mul ComplexVector Matrix ComplexVector where
 v <> m = v <> cm m
 
instance Mul Vector ComplexMatrix ComplexVector where
 v <> m = cv v <> m

--------------------------------- dot product

instance Mul Vector Vector Double where
 (<>) = vRvR
 
instance Mul ComplexVector ComplexVector (Complex Double) where
 (<>) = vCvC
  
instance Mul Vector ComplexVector (Complex Double) where
 a <> b = cv a <> b
 
instance Mul ComplexVector Vector (Complex Double) where
 (<>) = flip (<>)
 
--------------------------------- scaling vectors  
  
instance Mul Double Vector Vector where
 (<>) = scale

instance Mul Vector Double Vector where
 (<>) = flip (<>)
  
instance Mul (Complex Double) ComplexVector ComplexVector where
 (<>) = scaleC

instance Mul ComplexVector (Complex Double) ComplexVector where
 (<>) = flip (<>)

instance Mul Double ComplexVector ComplexVector where
 a <> v = (a:+0) <> v

instance Mul ComplexVector Double ComplexVector where
 (<>) = flip (<>)

instance Mul (Complex Double) Vector ComplexVector where
 a <> v = a <> cv v

instance Mul Vector (Complex Double) ComplexVector where
 (<>) = flip (<>)

--------------------------------- scaling matrices

instance Mul Double Matrix Matrix where
 (<>) a = asVector (a <>)

instance Mul Matrix Double Matrix where
 (<>) = flip (<>)

instance Mul (Complex Double) ComplexMatrix ComplexMatrix where
 (<>) a = asVector (a <>)

instance Mul ComplexMatrix (Complex Double) ComplexMatrix where
 (<>) = flip (<>)

instance Mul Double ComplexMatrix ComplexMatrix where
 a <> m = (a:+0) <> m

instance Mul ComplexMatrix Double ComplexMatrix where
 (<>) = flip (<>)

instance Mul (Complex Double) Matrix ComplexMatrix where
 a <> m = a <> cm m

instance Mul Matrix (Complex Double) ComplexMatrix where
 (<>) = flip (<>)

------------------------------- adding vectors

instance Add Vector Vector Vector where
 (|+|) = vectorZip 3

instance Add ComplexVector ComplexVector ComplexVector where
 (|+|) = vzip (+)
 
instance Add Vector ComplexVector ComplexVector where
 a |+| b = cv a |+| b
 
instance Add ComplexVector Vector ComplexVector where
 (|+|) = flip (|+|)

------------------------------- adding matrices

instance Add Matrix Matrix Matrix where
 (|+|) = asVector2 (|+|)

instance Add ComplexMatrix ComplexMatrix ComplexMatrix where
 (|+|) = asVector2 (|+|)
 
instance Add Matrix ComplexMatrix ComplexMatrix where
 (|+|) = asVector2 (|+|)
 
instance Add ComplexMatrix Matrix ComplexMatrix where
 (|+|) = asVector2 (|+|)

--------------------------------- adding scalars to vectors  
  
instance Add Double Vector Vector where
 (|+|) = offset

instance Add Vector Double Vector where
 (|+|) = flip (|+|)
  
instance Add (Complex Double) ComplexVector ComplexVector where
 (|+|) = offsetC

instance Add ComplexVector (Complex Double) ComplexVector where
 (|+|) = flip (|+|)

instance Add Double ComplexVector ComplexVector where
 a |+| v = (a:+0) |+| v

instance Add ComplexVector Double ComplexVector where
 (|+|) = flip (|+|)

instance Add (Complex Double) Vector ComplexVector where
 a |+| v = a |+| cv v

instance Add Vector (Complex Double) ComplexVector where
 (|+|) = flip (|+|)


--------------------------------- adding scalars to matrices 
  
instance Add Double Matrix Matrix where
 (|+|) a = asVector (a |+|)

instance Add Matrix Double Matrix where
 (|+|) = flip (|+|)
  
instance Add (Complex Double) ComplexMatrix ComplexMatrix where
 (|+|) a = asVector (a |+|)

instance Add ComplexMatrix (Complex Double) ComplexMatrix where
 (|+|) = flip (|+|)

instance Add Double ComplexMatrix ComplexMatrix where
 a |+| m = (a:+0) |+| m

instance Add ComplexMatrix Double ComplexMatrix where
 (|+|) = flip (|+|)

instance Add (Complex Double) Matrix ComplexMatrix where
 a |+| m = a |+| cm m

instance Add Matrix (Complex Double) ComplexMatrix where
 (|+|) = flip (|+|)

--------------------------------------
class Inv a b c | a b -> c where
 infixl 6 <\>
{- | Efficient multiplication by the inverse, without explicitly computing it. Useful for solving linear systems. It has the same meaning as the /left division/ operator of Matlab and GNU-Octave: @a \<\\> b = inv(a) \<\> b@. It is based on the 'lu' decomposition, /gsl_linalg_LU_solve/, and /gsl_linalg_complex_LU_solve/. Currently it only deals with square and nonsingular systems.

@a = 'realMatrix' [[1,1],[1,-1]]
b = 'realVector' [5,7]
\ 
\> a \<\\\> b
6. -1.
\ 
\> a \<\\\> 'complexVector' [-2,1+'i']
-0.500+0.500i  -1.500-0.500i@

 -}
 (<\>) :: a -> b -> c      

instance Inv Matrix Matrix Matrix where
 (<\>) = luSolveR

instance Inv ComplexMatrix ComplexMatrix ComplexMatrix where
 (<\>) = luSolveC

instance Inv Matrix ComplexMatrix ComplexMatrix where
 a <\> b = cm a <\> b

instance Inv ComplexMatrix Matrix ComplexMatrix where
 a <\> b = a <\> cm b

instance Inv Matrix Vector Vector where
 (<\>) a = flatten . (<\>) a . reshape 1 

instance Inv ComplexMatrix ComplexVector ComplexVector where
 (<\>) a = flatten . (<\>) a . reshape 1 

instance Inv ComplexMatrix Vector ComplexVector where
 a <\> b = a <\> cv b
 
instance Inv Matrix ComplexVector ComplexVector where
 a <\> b = cm a <\> b

--------------------------------------- general operations

{- | Shortcut for the 2-norm ('pnorm' 2)

@ > norm $ 'hilb' 5
1.5670506910982311
@

@\> norm $ 'complexVector' [1,-1,'i',-'i']
2.0@

-}
norm :: Norm a => a -> Double
norm x = pnorm 2 x

class Norm a where
 -- | computes the p-norm of a matrix or vector (with the same definitions as GNU-octave). pnorm 0 denotes \\inf-norm. See also 'norm'.
 pnorm :: Int -> a -> Double

instance Norm Vector where
 pnorm 2 = norm2
 pnorm 1 = norm1
 pnorm 0 = normInf . vectorMap 3
 pnorm _ = error "p norm not yet defined"

instance Norm ComplexVector where
 pnorm 2 = norm2 . asReal
 pnorm 1 = norm1 . vmap magnitude
 pnorm 0 = normInf . vmap magnitude
 pnorm _ = error "p norm not yet defined"

instance Norm Matrix where
 pnorm 2 m = head (toList s) where (_,s,_) = svd m
 pnorm 1 m = toScalar 4 $ constant 1 (rows m) <> asVector (vectorMap 3) m
 pnorm 0 m = toScalar 4 $ asVector (vectorMap 3) m <> constant 1 (cols m)
 pnorm _ _ = error "p norm not yet defined"

instance Norm ComplexMatrix where
 pnorm 2 m = maxvalsing m
  where maxvalsing m = sqrt . abs . head . toList . fst . eigH $ mm
        mm = if (rows m) > (cols m) then (conj.trans) m <> m
                                    else m <> (conj.trans) m
 pnorm 1 m = toScalar 4 $ constant 1 (rows m) <> asVector (vmap magnitude) m
 pnorm 0 m = toScalar 4 $ asVector (vmap magnitude) m <> constant 1 (cols m)
 pnorm _ _ = error "p norm not yet defined"


-- | svd of complex matrix, based on eigH
svdC :: ComplexMatrix -> (ComplexMatrix,Vector,ComplexMatrix)
svdC' m = (u',s,v) where
    (l,v) = eigH $ (conj.trans) m <> m
    s = (vmap (sqrt.abs)) l
    (_,u) = eigH $ m <> (conj.trans) m
    u' = fromColumns $ take (size s) $ toColumns u 
    
svdC x@(M r c _) = if r>=c 
    then svdC' x
    else (v, s, u) where (u,s,v) = svdC' (transC x)


-----------------------------------------------------------------

class Diag a r | a -> r where
    -- | Creates a diagonal matrix from a vector, or extracts the main diagonal from a matrix
    diag :: a -> r
    
instance Diag Vector Matrix where
    diag = diagR
instance Diag Matrix Vector where
    diag = takeDiag
instance Diag ComplexVector ComplexMatrix where
    diag = diagC
instance Diag ComplexMatrix ComplexVector where
    diag = takeDiagC
--instance Diag [Double] Matrix where
--    diag = diagR.fromList1    
        
----------------------------------------
        
class Constant dim res | dim -> res where  
    {- | Creates a constant real vector or matrix with the desired dimensions, or with the same dimensions as another vector or matrix. For example:
    
@\> constant 7.5 (5::Int)
7.500 7.500 7.500 7.500 7.500
\ 
\> constant 1 (2::Int,4::Int)
1.000 1.000 1.000 1.000
1.000 1.000 1.000 1.000
\ 
\> let m = 'realMatrix' [[1,2,4],[0,2,0]]
\> constant 3 m
3.000 3.000 3.000
3.000 3.000 3.000@

    -}
    constant :: Double -> dim -> res
     
        
instance Constant Int Vector where
    constant = constantV
               
instance Constant (Int,Int) Matrix where
    constant v (r,c) = constantM v r c
        
instance Constant Matrix Matrix where
    constant v (M r c _) = constantM v r c
    
instance Constant Vector Vector where
    constant v (V s _) = constantV v s
    


------------------------------------------

-- | objects which have a complex conjugate
class Conj obj where
    -- | complex conjugate of a complex vector or matrix
    conj :: obj (Complex Double) -> obj (Complex Double)
    
instance Conj GSLVector where
    conj = conjV
        
instance Conj GSLMatrix where
    conj = asVector conj       
     
-------------------------------------------
     
class Disp a where
 {- | Formatted version of a matrix or vector with n decimal places
 
@\> format 2 $ 0.5 + 'GSL.Derived.ident' 3
\"1.50 0.50 0.50\\n0.50 1.50 0.50\\n0.50 0.50 1.50\\n\"
\> putStr it
1.50 0.50 0.50
0.50 1.50 0.50
0.50 0.50 1.50@

 The Show instances of vectors and matrices use format 3. See also 'disp'.
 
 -}
 format :: Int -> a -> String
      
instance Disp Vector where
 format n = showVector " " (shf n)
       
instance Disp ComplexVector where
 format n = showVector "  " (shfc n)       
 
instance Disp Matrix where
 format n = showMatrix " " (shf n)
       
instance Disp ComplexMatrix where
 format n = showMatrix "  " (shfc n)
        
instance Show Vector where
    show = format 3

instance Show ComplexVector where
    show = format 3

instance Show Matrix where
    show = format 3

instance Show ComplexMatrix where
    show = format 3
        
----------------------------------------------
 
class SubMatrix mat where
    -- | extraction of a submatrix from a matrix
    subMatrix :: (Int,Int) -- ^ (initial row, initial column)
              -> (Int,Int) -- ^ (rows, columns) dimensions of submatrix
              -> mat -- ^ original matrix
              -> mat -- ^ resulting submatrix
           
instance SubMatrix Matrix where
 subMatrix = subMatrixR
 
instance SubMatrix ComplexMatrix where
 subMatrix = subMatrixC

------------------------------------------------

class Entrywise a b c | a b -> c where 
 {- | Element by element multiplication of vectors or matrices
 -}
 (.*) :: a -> b -> c
 
instance Entrywise Vector Vector Vector where
 (.*) = vzip (*)

instance Entrywise ComplexVector ComplexVector ComplexVector where
 (.*) = vzip (*)
   
instance Entrywise Vector ComplexVector ComplexVector where
 a .* b = cv a .* b
 
instance Entrywise ComplexVector Vector ComplexVector where
 a .* b = a .* cv b

instance Entrywise Matrix Matrix Matrix where
 (.*) = asVector2 (vzip (*))

instance Entrywise ComplexMatrix ComplexMatrix ComplexMatrix where
 (.*) = asVector2 (vzip (*))   
 
instance Entrywise Matrix ComplexMatrix ComplexMatrix where
 a .* b = cm a .* b
 
instance Entrywise ComplexMatrix Matrix ComplexMatrix where
 a .* b = a .* cm b
 
infixl 7 .*.
-- | multiplication by a Double (to avoid an explicit signature) 
(.*.) :: (Mul Double b c) => Double -> b -> c
(.*.) = (<>) 
 
gmap = undefined

extract l is = [l!!i |i<-is]

{- auxiliary function to get triangular matrices
-}
triang r c h v = reshape c $ fromList [el i j | i<-[0..r-1], j<-[0..c-1]]
    where el i j = if j-i>=h then v else (1::Double) - v
    
{- | rearranges the rows of a matrix according to the order given in a list of integers. 

> > extractRows [3,3,0,1] (ident 4)
> 0. 0. 0. 1.
> 0. 0. 0. 1.
> 1. 0. 0. 0.
> 0. 1. 0. 0.

-}
extractRows :: (Storable t) => [Int] -> GSLMatrix t -> GSLMatrix t
extractRows l m = fromRows $ extract (toRows $ m) l
    
class LU t where
 {- | The LU decomposition of a square matrix. Is based on /gsl_linalg_LU_decomp/ and  /gsl_linalg_complex_LU_decomp/ as described in <http://www.gnu.org/software/gsl/manual/gsl-ref_13.html#SEC223>.

@\> let m = 'complexMatrix' [[1,2,-3],[2+3*'i',-7,0],[1,-'i',2*'i']]
\> let (l,u,p,s) = lu m@

L is the lower triangular:

@\> l
          1.            0.  0.
0.154-0.231i            1.  0.
0.154-0.231i  0.624-0.522i  1.@

U is the upper triangular:

@\> u
2.+3.i           -7.            0.
    0.  3.077-1.615i           -3.
    0.            0.  1.873+0.433i@

p is a permutation:

@\> p
[1,0,2]@

L \* U obtains a permuted version of the original matrix:

@\> 'extractRows' p m
  2.+3.i   -7.   0.
      1.    2.  -3.
      1.  -1.i  2.i
\ 
\> l \<\> u
 2.+3.i   -7.   0.
     1.    2.  -3.
     1.  -1.i  2.i@

s is the sign of the permutation, required to obtain sign of the determinant:

@\> s * product ('toList' $ 'diag' u)
(-18.0) :+ (-16.000000000000004)
\> 'det' m
(-18.0) :+ (-16.000000000000004)@

 -}
 lu :: GSLMatrix t -> (GSLMatrix t, GSLMatrix t, [Int], t)

instance LU Double where
 lu m@(M r _ _) = (l,u,p, fromIntegral s') where
    v = luRaux m
    lu = reshape r $ subVector 0 (r*r) v
    s':p = map round . toList . subVector (r*r) (r+1) $ v 
    u = triang r r 0 1 .* lu
    l = triang r r 0 0 .* lu |+| ident r

instance LU (Complex Double) where
 lu m@(M r _ _) = (l,u,p, fromIntegral s') where
    v = luCaux m
    lu = reshape r $ subVector 0 (r*r) v
    s':p = map (round.realPart) . toList . subVector (r*r) (r+1) $ v 
    u = triang r r 0 1 .* lu
    l = triang r r 0 0 .* lu |+| ident r

{- | Determinant of a square matrix, computed from the 'lu' decomposition.

@\> det ('realMatrix' [[7,2],[3,8]])
50.0@

-}
det :: (Diag (GSLMatrix t) (GSLVector t), (FromToList (GSLVector t) [t]), Num t, LU t) => GSLMatrix t -> t
det m = s * (product $ toList $ diag $ u) 
    where (_,u,_,s) = lu m 
    
{- | fast 1D Fourier transform of a vector using /gsl_fft_complex_forward/. It uses the same scaling conventions as GNU Octave.

@> fft ('complexVector' [1,2,3,4])
10.  -2.+2.i  -2.  -2.-2.i@

-}
fft :: ComplexVector -> ComplexVector
fft = genfft 0

{- | inverse 'fft' using /gsl_fft_complex_inverse/.

@> ifft ('complexVector' [0,2-2*'i',0,2+2*'i'])
1.  1.  -1.  -1.@

-}
ifft :: ComplexVector->ComplexVector
ifft = genfft 1
            
-----------------------------------------------------------

class Eig t where
 {- | Eigenvalues and eigenvectors of a real symmetric matrix, using /gsl_eigen_symmv/:

@\> let (l,v) = eig ('realMatrix' [[1,2],[2,1]])
\> l
3.000 -1.000
\ 
\> v
0.707 -0.707
0.707  0.707
\ 
\> v \<\> 'diag' l \<\> 'trans' v
1.000 2.000
2.000 1.000@

Eigenvalues and eigenvectors of a complex hermitian matrix, using /gsl_eigen_hermv/:

@\> let (l,v) = eig $ 'complexMatrix' [[1,2+'i'],[2-'i',3]]
\ 
\> l
4.449 -0.449
\ 
\> v
       -0.544          0.839
-0.751+0.375i  -0.487+0.243i
\ 
\> v \<\> 'diag' l \<\> ('conj' . 'trans') v
    1.  2.+1.i
2.-1.i      3.@

-}
 eig :: GSLMatrix t -> (GSLVector Double, GSLMatrix t)       
 
instance Eig Double where
 eig m | isSymmetric m = eigS m
       | otherwise     = error "eig received a nonsymmetric real matrix"
 
instance Eig (Complex Double) where
 eig m | isHermitian m = eigH m 
       | otherwise     = error "eig received a nonhermitian complex matrix"    
       
      
isSymmetric m = isSquare m && pnorm 1 (flatten $ m |-| (trans m)) < 1e-10

isHermitian m = isSquare m && pnorm 1 (flatten $ m |-| (conj. trans $ m)) < 1e-10
    
    
------------------------------------------------------------

-- | Creates a real vector from a list.
realVector :: [Double] -> Vector
realVector = fromList

-- | Creates a complex vector from a list.
complexVector :: [Complex Double] -> ComplexVector
complexVector = fromList

-- | Creates a real matrix from a list of lists.
realMatrix :: [[Double]] -> Matrix
realMatrix = fromList

-- | Creates a complex matrix from a list of lists. 
complexMatrix :: [[Complex Double]] -> ComplexMatrix
complexMatrix = fromList

{- | postfix function application with low precedence (as in Mathematica)

@\> 'hilb' 10 \/\/ 'eig' \/\/ fst \/\/ 'toList' \/\/ maximum
1.7519196702651774@

-}
(//) :: a -> (a -> b) -> b
infixl 1 //
x // f = f x
------------------------------------------    

-- | Reverse rows 
flipud :: Storable t => GSLMatrix t -> GSLMatrix t
flipud m = fromRows . reverse . toRows $ m

-- | Reverse columns
fliprl :: (Storable t, Trans t) => GSLMatrix t -> GSLMatrix t
fliprl m = fromColumns . reverse . toColumns $ m   

{- | Prints a formatted matrix or vector with n digits after the decimal point.
-}
disp :: (Disp a) => Int -> a -> IO ()
disp n = putStrLn . format n

--------------------------------------------

-- | Sum of columns of a matrix.
sumColumns :: (Mul Vector (GSLMatrix t) (GSLVector t)) => GSLMatrix t -> GSLVector t
sumColumns m = constant 1 (rows m) <> m

{- | Outer product of two vectors.

@\> 'realVector' [1,2,3] \`outer\` 'complexVector' [7,0,2*'i',1+'i']
 7.  0.  2.i  1.+1.i
14.  0.  4.i  2.+2.i
21.  0.  6.i  3.+3.i@

-}
outer :: (Mul (GSLMatrix a) (GSLMatrix b) (GSLMatrix r)) => GSLVector a -> GSLVector b -> GSLMatrix r
outer u v = reshape 1 u <> reshape (size v) v

-------------------------------------------------

--------------------------------------------------------------
    
{- | Pseudoinverse of a real matrix with the default tolerance used by GNU-Octave: the singular values less than max (rows, colums) * greatest singular value * 'eps' are ignored. See 'pinvTol'.

@\> let m = 'realMatrix' [[1,2],[5,8],[10,-5]]
\> pinv m
9.353e-3 4.539e-2  7.637e-2
2.231e-2 8.993e-2 -4.719e-2
\ 
\> m \<\> pinv m \<\> m
 1.  2.
 5.  8.
10. -5.@

-}
pinv :: Matrix -> Matrix
pinv m = pinvTol 1 m

{- | Pseudoinverse of a real matrix with the desired tolerance, expressed as a
multiplicative factor of the default tolerance used by GNU-Octave (see 'pinv').

@\> let m = 'realMatrix' [[1,0,0],[0,1,0],[0,0,1e-10]]
\ 
\> 'pinv' m 
1. 0.           0.
0. 1.           0.
0. 0. 10000000000.
\ 
\> pinvTol 1E8 m
1. 0. 0.
0. 1. 0.
0. 0. 1.@

-}
pinvTol :: Double -> Matrix -> Matrix
pinvTol t m = v <> diag s' <> trans u where
    (u,s,v) = svd m
    sl@(g:_) = toList s
    s' = fromList . map rec $ sl
    rec x = if x < g*tol then 1 else 1/x
    tol = (fromIntegral (max (rows m) (cols m)) * g * t * eps)
    
-------------------------------------------------------------------------    
{- | The method of Nelder and Mead, implemented by /gsl_multimin_fminimizer_nmsimplex/. The gradient of the function is not required. This is the example in the GSL manual:

@minimize f xi = minimizeNMSimplex f xi (replicate (length xi) 1) 1e-6 100
\ 
f [x,y] = 10*(x-1)^2 + 20*(y-2)^2 + 30
\ 
main = do
    let (s,p) = minimize f [5,7]
    print s
    print p
\ 
\> main
[0.9920430849306285,1.9969168063253164]
 0. 512.500       0. 6.500    5.
 1. 290.625    1.082 5.250    4.
 2. 290.625    1.372 5.250    4.
 3. 252.500    1.372 5.500    1.
 4. 101.406    1.372 2.625 3.500
 5. 101.406    1.823 2.625 3.500
 6.     60.    1.823    0.    3.
 7.  42.275    1.823 2.094 1.875
 8.  42.275    1.303 2.094 1.875
 9.  35.684    1.303 0.258 1.906
10.  35.664    1.026 0.588 2.445
11.  30.680    0.804 1.258 2.025
12.  30.680    0.467 1.258 2.025
13.  30.539    0.356 1.093 1.849
14.  30.137    0.285 0.883 2.004
15.  30.137    0.168 0.883 2.004
16.  30.090    0.123 0.958 2.060
17.  30.005    0.100 1.022 2.004
18.  30.005 6.051e-2 1.022 2.004
19.  30.005 4.249e-2 1.022 2.004
20.  30.005 4.249e-2 1.022 2.004
21.  30.005 2.742e-2 1.022 2.004
22.  30.001 2.119e-2 0.992 1.997
23.  30.001 1.530e-2 0.992 1.997
24.  30.001 1.259e-2 0.992 1.997@    

The path to the solution can be graphically shown by means of:

@'GSL.Drawing.hplot' $ drop 3 ('toColumns' p)@

-}     
minimizeNMSimplex :: ([Double] -> Double) -- ^ function to minimize
          -> [Double]            -- ^ starting point
          -> [Double]            -- ^ sizes of the initial search box
          -> Double              -- ^ desired precision of the solution
          -> Int                 -- ^ maximum number of iterations allowed
          -> ([Double], Matrix)   
          -- ^ solution vector, and the optimization trajectory followed by the algorithm      
minimizeNMSimplex f xi sz tol maxit = (sol, path) where
    rawpath = minimizeV (f.toList) tol maxit (fromList xi) (fromList sz)
    it = round (rawpath !!: (maxit-1,0))
    path = takeRows it rawpath
    [sol] = toList $ dropRows (it-1) path

{- | The Fletcher-Reeves conjugate gradient algorithm /gsl_multimin_fminimizer_conjugate_fr/. This is the example in the GSL manual:

@minimize f df xi = minimizeConjugateGradient 1E-2 1E-4 1E-3 30 
\                                             (f . 'toList') 
\                                             ('fromList' . df . 'toList') 
\                                             ('fromList' xi)
f [x,y] = 10*(x-1)^2 + 20*(y-2)^2 + 30
\ 
df [x,y] = [20*(x-1), 40*(y-2)]
\  
main = do
    let (s,p) = minimize f df [5,7]
    print s
    print p
\ 
\> main
[1.0,2.0]
 0. 687.848 4.996 6.991
 1. 683.555 4.989 6.972
 2. 675.013 4.974 6.935
 3. 658.108 4.944 6.861
 4. 625.013 4.885 6.712
 5. 561.684 4.766 6.415
 6. 446.467 4.528 5.821
 7. 261.794 4.053 4.632
 8.  75.498 3.102 2.255
 9.  67.037 2.852 1.630
10.  45.316 2.191 1.762
11.  30.186 0.869 2.026
12.     30.    1.    2.@

The path to the solution can be graphically shown by means of:

@'GSL.Drawing.hplot' $ drop 2 ('toColumns' p)@

-}     
minimizeConjugateGradient :: 
       Double        -- ^ initial step size
    -> Double        -- ^ minimization parameter   
    -> Double        -- ^ desired precision of the solution (gradient test)
    -> Int           -- ^ maximum number of iterations allowed
    -> (Vector -> Double) -- ^ function to minimize
    -> (Vector -> Vector)      -- ^ gradient  
    -> Vector             -- ^ starting point
    -> (Vector, Matrix)        -- ^ solution vector, and the optimization trajectory followed by the algorithm      
minimizeConjugateGradient istep minimpar tol maxit f df xi = (sol, path) where
    rawpath = minimizeDerivV f df tol maxit xi istep minimpar
    it = round (rawpath !!: (maxit-1,0))
    path = takeRows it rawpath
    sol = flatten $ dropRows (it-1) path

----------------------------------------------------------

class Comp a b | a -> b where
    -- | Creates a complex version of an entity.
    complex :: a -> b

instance Comp Double (Complex Double) where
    complex = (:+ 0)

instance Comp (Complex Double) (Complex Double) where
    complex = id

instance Comp Vector ComplexVector where
    complex = cv

instance Comp ComplexVector ComplexVector where
    complex = id

instance Comp Matrix ComplexMatrix where
    complex = cm

instance Comp ComplexMatrix ComplexMatrix where
    complex = id

------------------------------------------------------------

class FromArray gsl_object haskell_array | haskell_array -> gsl_object{-, gsl_object-> haskell_array-} where
    {- | Creates a vector or a matrix from a standard Haskell @Array@ or @StorableArray@. See also 'toArray'. For example:

@type MyHaskellVector  = Array (Int) (Complex Double)
type MyHaskellMatrix  = StorableArray (Int,Int) Double
\ 
hv,chv :: MyHaskellVector
\ 
hv = listArray (1,4) [1,0:+1,2:+3,5:+(-1)] 
chv = 'toArray'. 'conj' . 'fromArray' $ hv
\ 
main = do 
    print (elems chv)
    print $ norm ('fromArray' hv)
\    
    hm <- newListArray ((1,1),(3,5)) [1 .. 15] :: IO MyHaskellMatrix
    m <- 'fromArray' hm
    print m
    mn <- 'toArray' ('trans' m) :: IO MyHaskellMatrix
    em <- getElems mn
    print em
\ 
\>  main
[1.0 :+ 0.0,0.0 :+ (-1.0),2.0 :+ (-3.0),5.0 :+ 1.0]
6.4031242374328485
\ 
 1.  2.  3.  4.  5.
 6.  7.  8.  9. 10.
11. 12. 13. 14. 15.
\ 
[1.0,6.0,11.0,2.0,7.0,12.0,3.0,8.0,13.0,4.0,9.0,14.0,5.0,10.0,15.0]@

For StorableArray the elements are efficient copied using @withStorableArray@ and @copyArray@. For Array we @freeze@ or @thaw@ an auxiliary @StorableArray@.

-}
    fromArray :: haskell_array -> gsl_object
         
instance FromArray (IO Vector) (StorableArray Int Double) where
    fromArray = fromStorableArrayV
    
instance FromArray (IO ComplexVector) (StorableArray Int (Complex Double)) where
    fromArray = fromStorableArrayV
  
instance FromArray (IO Matrix) (StorableArray (Int,Int) Double) where
    fromArray = fromStorableArrayM
    
instance FromArray (IO (ComplexMatrix)) (StorableArray (Int,Int) (Complex Double)) where
    fromArray = fromStorableArrayM

instance FromArray Vector (Array Int Double) where
    fromArray = fromArrayV

instance FromArray ComplexVector (Array Int (Complex Double)) where
    fromArray = fromArrayV

instance FromArray Matrix (Array (Int,Int) Double) where
    fromArray = fromArrayM

instance FromArray ComplexMatrix (Array (Int,Int) (Complex Double)) where
    fromArray = fromArrayM


---------------------------------------------------------------------------------

class ToArray gsl_object haskell_array | haskell_array -> gsl_object{-, gsl_object-> haskell_array-} where
    {- | Creates a standard Haskell @Array@ or @StorableArray@ from a vector or a matrix. See 'fromArray'.

-}    
    toArray :: gsl_object -> haskell_array
         
instance ToArray Vector (IO (StorableArray Int Double)) where
    toArray = toStorableArrayV
    
instance ToArray ComplexVector (IO (StorableArray Int (Complex Double))) where
    toArray = toStorableArrayV
  
instance ToArray Matrix (IO (StorableArray (Int,Int) Double)) where
    toArray = toStorableArrayM
    
instance ToArray ComplexMatrix (IO (StorableArray (Int,Int) (Complex Double))) where
    toArray = toStorableArrayM

instance ToArray Vector (Array Int Double) where
    toArray = toArrayV

instance ToArray ComplexVector (Array Int (Complex Double)) where
    toArray = toArrayV

instance ToArray Matrix (Array (Int,Int) Double) where
    toArray = toArrayM

instance ToArray ComplexMatrix (Array (Int,Int) (Complex Double)) where
    toArray = toArrayM

----------------------------------------------------------------------------

class FromToList gsl_object list | list -> gsl_object, gsl_object->list where
    {- | Creates a vector from a list of numbers, or a matrix from a list of lists of numbers, considered as rows. We have the specialized versions 'realVector', 'realMatrix', 'complexVector', and 'complexMatrix', as an alternative to type annotations. Vectors and matrices can also be created from some types of standard Haskell arrays using 'fromArray'.
    -}
    fromList :: list -> gsl_object
    {- | Creates a list of numbers from a vector, or a list of lists of numbers (the rows), from a matrix. Some types of standard Haskell arrays can be also obtained from vectors or matrices using 'toArray'.
    -}
    toList :: gsl_object -> list
 
instance FromToList Vector [Double] where
    fromList = fromList1
    toList = toList1
 
instance FromToList ComplexVector [Complex Double] where
    fromList = fromList1
    toList = toList1
      
instance FromToList Matrix [[Double]] where
    fromList = fromList2
    toList = toList2
 
instance FromToList ComplexMatrix [[Complex Double]] where
    fromList = fromList2
    toList = toList2

-------------------------------------------------
-- | Creates a matrix with the first n rows of another matrix
takeRows :: SubMatrix (GSLMatrix t) => Int -> GSLMatrix t -> GSLMatrix t
takeRows n mat = subMatrix (0,0) (n, cols mat) mat
-- | Creates a copy of a matrix without the first n rows
dropRows :: SubMatrix (GSLMatrix t) => Int -> GSLMatrix t -> GSLMatrix t
dropRows n mat = subMatrix (n,0) (rows mat - n, cols mat) mat
-- |Creates a matrix with the first n columns of another matrix
takeColumns :: SubMatrix (GSLMatrix t) => Int -> GSLMatrix t -> GSLMatrix t
takeColumns n mat = subMatrix (0,0) (rows mat, n) mat
-- | Creates a copy of a matrix without the first n columns
dropColumns :: SubMatrix (GSLMatrix t) => Int -> GSLMatrix t -> GSLMatrix t
dropColumns n mat = subMatrix (0,n) (rows mat, cols mat - n) mat
{-# OPTIONS -fglasgow-exts #-}

module GSL.Interface where

import GSL.Base
import GSL.Wrappers
import GSL.Derived
import Complex
import Data.List(transpose)
import Numeric(showGFloat)
import Foreign


{- | the imaginary unit

@> ident 3 \<\> i
1.i   0.   0.
 0.  1.i   0.
 0.   0.  1.i@

-}
i :: Complex Double
i = 0:+1 



class Mul a b c | a b -> c where
 infixl 7 <>
 -- | matrix product, matrix-vector product, dot product and scaling of vectors and matrices
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

cv v = complexV (v, (0::Double)<>v)
cm m = complexM (m, (0::Double)<>m)

-- | map on vectors
vmap :: (Storable a, Storable b) => (a -> b) -> Vector a -> Vector b
vmap f = fromList . map f . toList

-- | zipWith on vectors
vzip :: (Storable a, Storable b, Storable c) => (a -> b -> c) -> Vector a -> Vector b -> Vector c
vzip f a b = fromList $ zipWith f (toList a) (toList b)

-- | map on matrices
mmap :: (Storable a, Storable b) => (a -> b) -> Matrix a -> Matrix b
mmap m = asVector . vmap $ m

-- zipWith on matrices
mzip :: (Storable a, Storable b, Storable c) => (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
mzip f = asVector2 (vzip f)

--------------------------------- matrix matrix

instance Mul M M M where
 (<>) = multiply

instance Mul CM CM CM where
 (<>) = multiplyC

instance Mul CM M CM where
 c <> r = c <> cm r

instance Mul M CM CM where
 r <> c = cm r <> c

--------------------------------- matrix vector

instance Mul M V V where
 (<>) = mRvR

instance Mul CM CV CV where
 (<>) = mCvC

instance Mul CM V CV where
 m <> v = m <> cv v

instance Mul M CV CV where
 m <> v = cm m <> v

--------------------------------- vector matrix

instance Mul V M V where
 (<>) = vRmR
 
instance Mul CV CM CV where
 (<>) = vCmC
 
instance Mul CV M CV where
 v <> m = v <> cm m
 
instance Mul V CM CV where
 v <> m = cv v <> m

--------------------------------- dot product

instance Mul V V Double where
 (<>) = vRvR
 
instance Mul CV CV (Complex Double) where
 (<>) = vCvC
  
instance Mul V CV (Complex Double) where
 a <> b = cv a <> b
 
instance Mul CV V (Complex Double) where
 (<>) = flip (<>)
 
--------------------------------- scaling vectors  
  
instance Mul Double V V where
 (<>) = scale

instance Mul V Double V where
 (<>) = flip (<>)
  
instance Mul (Complex Double) CV CV where
 (<>) = scaleC

instance Mul CV (Complex Double) CV where
 (<>) = flip (<>)

instance Mul Double CV CV where
 a <> v = (a:+0) <> v

instance Mul CV Double CV where
 (<>) = flip (<>)

instance Mul (Complex Double) V CV where
 a <> v = a <> cv v

instance Mul V (Complex Double) CV where
 (<>) = flip (<>)

--------------------------------- scaling matrices

instance Mul Double M M where
 (<>) a = asVector (a <>)

instance Mul M Double M where
 (<>) = flip (<>)

instance Mul (Complex Double) CM CM where
 (<>) a = asVector (a <>)

instance Mul CM (Complex Double) CM where
 (<>) = flip (<>)

instance Mul Double CM CM where
 a <> m = (a:+0) <> m

instance Mul CM Double CM where
 (<>) = flip (<>)

instance Mul (Complex Double) M CM where
 a <> m = a <> cm m

instance Mul M (Complex Double) CM where
 (<>) = flip (<>)

------------------------------- adding vectors

instance Add V V V where
 (|+|) = vectorZip 3

instance Add CV CV CV where
 (|+|) = vzip (+)
 
instance Add V CV CV where
 a |+| b = cv a |+| b
 
instance Add CV V CV where
 (|+|) = flip (|+|)

------------------------------- adding matrices

instance Add M M M where
 (|+|) = asVector2 (|+|)

instance Add CM CM CM where
 (|+|) = asVector2 (|+|)
 
instance Add M CM CM where
 (|+|) = asVector2 (|+|)
 
instance Add CM M CM where
 (|+|) = asVector2 (|+|)

--------------------------------- adding scalars to vectors  
  
instance Add Double V V where
 (|+|) = offset

instance Add V Double V where
 (|+|) = flip (|+|)
  
instance Add (Complex Double) CV CV where
 (|+|) = offsetC

instance Add CV (Complex Double) CV where
 (|+|) = flip (|+|)

instance Add Double CV CV where
 a |+| v = (a:+0) |+| v

instance Add CV Double CV where
 (|+|) = flip (|+|)

instance Add (Complex Double) V CV where
 a |+| v = a |+| cv v

instance Add V (Complex Double) CV where
 (|+|) = flip (|+|)


--------------------------------- adding scalars to matrices 
  
instance Add Double M M where
 (|+|) a = asVector (a |+|)

instance Add M Double M where
 (|+|) = flip (|+|)
  
instance Add (Complex Double) CM CM where
 (|+|) a = asVector (a |+|)

instance Add CM (Complex Double) CM where
 (|+|) = flip (|+|)

instance Add Double CM CM where
 a |+| m = (a:+0) |+| m

instance Add CM Double CM where
 (|+|) = flip (|+|)

instance Add (Complex Double) M CM where
 a |+| m = a |+| cm m

instance Add M (Complex Double) CM where
 (|+|) = flip (|+|)

--------------------------------------
class Inv a b c | a b -> c where
 infixl 6 <\>
{- | Efficient multiplication by the inverse, without explicitly computing it. Useful for solving linear systems. It has the same meaning as the /left division/ operator of Matlab and GNU-octave: 

 @a \<\\> b = inverse(a) \<\> b@

 It is based on the 'lu' decomposition, /gsl_linalg_LU_solve/, and /gsl_linalg_complex_LU_solve/. Currently it only deals with square and nonsingular systems.

 -}
 (<\>) :: a -> b -> c      

instance Inv M M M where
 (<\>) = luSolveR

instance Inv CM CM CM where
 (<\>) = luSolveC

instance Inv M CM CM where
 a <\> b = cm a <\> b

instance Inv CM M CM where
 a <\> b = a <\> cm b

instance Inv M V V where
 (<\>) a = flatten . (<\>) a . reshape 1 

instance Inv CM CV CV where
 (<\>) a = flatten . (<\>) a . reshape 1 

instance Inv CM V CV where
 a <\> b = a <\> cv b
 
instance Inv M CV CV where
 a <\> b = cm a <\> b

--------------------------------------- general operations

{- | shortcut for the 2-norm ('pnorm' \"2\")

@ > norm $ 'hilb' 5
 1.5670506910982311
@

> > norm $ fromList [1,-1,i,-i]
> 2.0

-}
norm :: Norm a => a -> Double
norm x = pnorm "2" x

class Norm a where
 -- | computes the p-norm of a matrix or vector (with the same definitions as GNU-octave) . See also 'norm'.
 pnorm :: String -> a -> Double

instance Norm V where
 pnorm "2" = norm2
 pnorm "1" = norm1
 pnorm "inf" = normInf . vectorMap 3
 pnorm _ = error "p norm not defined"

instance Norm CV where
 pnorm "2" = norm2 . asReal
 pnorm "1" = norm1 . vmap magnitude
 pnorm "inf" = normInf . vmap magnitude
 pnorm _ = error "p norm not defined"

instance Norm M where
 pnorm "2" m = head (toList s) where (_,s,_) = svd m
 pnorm "1" m = toScalar 4 $ constant 1 (rows m) <> asVector (vectorMap 3) m
 pnorm "inf" m = toScalar 4 $ asVector (vectorMap 3) m <> constant 1 (cols m)
 pnorm _ _ = error "p norm not defined"

instance Norm CM where
 pnorm "2" m = maxvalsing m
  where maxvalsing m = sqrt . abs . head . toList . fst . eigH $ mm
        mm = if (rows m) > (cols m) then (conj.trans) m <> m
                                    else m <> (conj.trans) m
 pnorm "1" m = toScalar 4 $ constant 1 (rows m) <> asVector (vmap magnitude) m
 pnorm "inf" m = toScalar 4 $ asVector (vmap magnitude) m <> constant 1 (cols m)
 pnorm _ _ = error "p norm not defined"


-- | svd of complex matrix, based on eigH
svdC :: CM -> (CM,V,CM)
svdC' m = (u',s,v) where
    (l,v) = eigH $ (conj.trans) m <> m
    s = (vmap (sqrt.abs)) l
    (_,u) = eigH $ m <> (conj.trans) m
    u' = fromCols $ take (size s) $ toCols u 
    
svdC x@(M r c _) = if r>=c 
    then svdC' x
    else (v, s, u) where (u,s,v) = svdC' (transC x)


-----------------------------------------------------------------

class Diag a r | a -> r where
    -- | create a diagonal matrix from a vector, or extract the main diagonal from a matrix
    diag :: a -> r
    
instance Diag V M where
    diag = diagR
instance Diag M V where
    diag = takeDiag
instance Diag CV CM where
    diag = diagC
instance Diag CM CV where
    diag = takeDiagC
--instance Diag [Double] M where
--    diag = diagR.fromList    
        
----------------------------------------
        
class Constant dim res | dim -> res where  
    {- | creates a constant real vector or matrix with the desired dimensions, or with the same dimensions as another vector or matrix. For example:
    
    > > constant 7.5 (5::Int)
    > 7.500 7.500 7.500 7.500 7.500
    >
    > > constant 1 (2::Int,4::Int)
    > 1.000 1.000 1.000 1.000
    > 1.000 1.000 1.000 1.000
    >
    > > let m = fromLists [[1,2,4],[0,2,0]] :: M
    > > constant 3 m
    > 3.000 3.000 3.000
    > 3.000 3.000 3.000
    >
    -}
    constant :: Double -> dim -> res
     
        
instance Constant Int V where
    constant = constantV
               
instance Constant (Int,Int) M where
    constant v (r,c) = constantM v r c
        
instance Constant M M where
    constant v (M r c _) = constantM v r c
    
instance Constant V V where
    constant v (V s _) = constantV v s
    


------------------------------------------

-- | objects which have a complex conjugate
class Conj obj where
    -- | complex conjugate of a complex vector or matrix
    conj :: obj (Complex Double) -> obj (Complex Double)
    
instance Conj Vector where
    conj = conjV
        
instance Conj Matrix where
    conj = asVector conj       
     
-------------------------------------------
     
class Disp a where
 {- | formatted version of a matrix or vector with n decimal places
 
 > > format 2 $ (0.5::Double) |+| ident 3
 > "1.50 0.50 0.50\n0.50 1.50 0.50\n0.50 0.50 1.50\n"
 > > putStr it
 > 1.50 0.50 0.50
 > 0.50 1.50 0.50
 > 0.50 0.50 1.50

 The Show instances of vectors and matrices use format 3. See also 'disp'.
 
 -}
 format :: Int -> a -> String
      
instance Disp V where
 format n = showVector " " (shf n)
       
instance Disp CV where
 format n = showVector "  " (shfc n)       
 
instance Disp M where
 format n = showMatrix " " (shf n)
       
instance Disp CM where
 format n = showMatrix "  " (shfc n)
        
instance Show V where
    show = format 3

instance Show CV where
    show = format 3

instance Show M where
    show = format 3

instance Show CM where
    show = format 3
        
----------------------------------------------
 
class SubMatrix mat where
    -- | extraction of a submatrix from a matrix
    subMatrix :: Int -- ^ first row 
          -> Int -- ^ last row
          -> Int -- ^ first column
          -> Int -- ^ last column
          -> mat -- ^ original matrix
          -> mat -- ^ resulting submatrix
           
instance SubMatrix M where
 subMatrix = subMatrixR
 
instance SubMatrix CM where
 subMatrix = subMatrixC

------------------------------------------------

class Entrywise a b c | a b -> c where 
 {- | Element by element multiplication of vectors or matrices
 -}
 (.*) :: a -> b -> c
 
instance Entrywise V V V where
 (.*) = vzip (*)

instance Entrywise CV CV CV where
 (.*) = vzip (*)
   
instance Entrywise V CV CV where
 a .* b = cv a .* b
 
instance Entrywise CV V CV where
 a .* b = a .* cv b

instance Entrywise M M M where
 (.*) = asVector2 (vzip (*))

instance Entrywise CM CM CM where
 (.*) = asVector2 (vzip (*))   
 
instance Entrywise M CM CM where
 a .* b = cm a .* b
 
instance Entrywise CM M CM where
 a .* b = a .* cm b
 
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
extractRows :: (Storable t) => [Int] -> Matrix t -> Matrix t
extractRows l m = fromRows $ extract (toRows $ m) l
    
class LU t where
 {- | The LU decomposition of a square matrix. Is based on /gsl_linalg_LU_decomp/ and  /gsl_linalg_complex_LU_decomp/ as described in <http://www.gnu.org/software/gsl/manual/gsl-ref_13.html#SEC223>.

> > let m = fromLists [[1,2,-3],[2+3*i,-7,0],[1,-i,2*i]]
> > let (l,u,p,s) = lu m

L is the lower triangular:

> > l
>           1.            0.  0.
> 0.154-0.231i            1.  0.
> 0.154-0.231i  0.624-0.522i  1.

U is the upper triangular:

>
> > u
> 2.+3.i           -7.            0.
>     0.  3.077-1.615i           -3.
>     0.            0.  1.873+0.433i

p is a permutation:

> > p
> [1,0,2]

L \* U obtains a permuted version of the original matrix:

@ > 'extractRows' p m
  2.+3.i   -7.   0.
      1.    2.  -3.
      1.  -1.i  2.i @

@ > l \<\> u
 2.+3.i   -7.   0.
     1.    2.  -3.
     1.  -1.i  2.i @

s is the sign of the permutation, required to obtain sign of the determinant:

> > s * product (toList $ diag u)
> (-18.0) :+ (-16.000000000000004)
> > det m
> (-18.0) :+ (-16.000000000000004)

 -}
 lu :: Matrix t -> (Matrix t, Matrix t, [Int], t)

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

{- | determinant of a square matrix, computed from the 'lu' decomposition.

> > let m = fromLists [[7,2],[3,8]] ::M
> (0.01 secs, 543320 bytes)
> det m
> 50.0

-}
det :: (Diag (Matrix t) (Vector t), Storable t, Num t, LU t) => Matrix t -> t
det m = s * (product $ toList $ diag $ u) 
    where (_,u,_,s) = lu m 
    
{- | fast 1D Fourier transform of a vector using /gsl_fft_complex_forward/. It uses the same scaling conventions as GNU Octave.

>> fft (fromList [1,2,3,4])
> 10.  -2.+2.i  -2.  -2.-2.i

-}
fft :: CV -> CV
fft = genfft 0

{- | inverse fft using /gsl_fft_complex_inverse/.

> > ifft (fromList [0,2-2*i,0,2+2*i])
> 1.  1.  -1.  -1.

-}
ifft :: CV->CV
ifft = genfft 1
            
-----------------------------------------------------------

class Eig t where
 {- | eigendecomposition of a real symmetric matrix using /gsl_eigen_symmv/.

> > let (l,v) = eig (fromLists [[1,2],[2,1]] ::M)
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

eigendecomposition of a complex hermitian matrix using /gsl_eigen_hermv/

> > let (l,v) = eig $ fromLists [[1,2+i],[2-i,3]]
>
> > l
> 4.449 -0.449
>
> > v
>        -0.544          0.839
> -0.751+0.375i  -0.487+0.243i
>
> > v <> diag l <> (conj.trans) v
>     1.  2.+1.i
> 2.-1.i      3.

-}


 eig :: Matrix t -> (Vector Double, Matrix t)       
 
instance Eig Double where
 eig m | isSymmetric m = eigS m
       | otherwise     = error "eig received a nonsymmetric real matrix"
 
instance Eig (Complex Double) where
 eig m | isHermitian m = eigH m 
       | otherwise     = error "eig received a nonhermitian complex matrix"    
       
      
isSymmetric m = isSquare m && pnorm "1" (flatten $ m |-| (trans m)) < 1e-10

isHermitian m = isSquare m && pnorm "1" (flatten $ m |-| (conj. trans $ m)) < 1e-10
    
    
------------------------------------------------------------

-- | creates a real vector from a list. Useful in some situations to avoid type annotations.
realVector :: [Double] -> V
realVector = fromList

-- | creates a complex vector from a list. Useful in some situations to avoid type annotations.
complexVector :: [Complex Double] -> CV
complexVector = fromList

-- | creates a real vector from a list of lists. Useful in some situations to avoid type annotations.
realMatrix :: [[Double]] -> M
realMatrix = fromLists

-- | creates a complex vector from a list. Useful in some situations to avoid type annotations.
complexMatrix :: [[Complex Double]] -> CM
complexMatrix = fromLists

{- | postfix function application with low precedence (as in Mathematica)

> > hilb 10 // eig // fst // toList // maximum
> 1.7519196702651774

-}
(//) :: a -> (a -> b) -> b
infixl 1 //
x // f = f x
------------------------------------------    

-- | reverse rows 
flipud :: Storable t => Matrix t -> Matrix t
flipud m = fromRows . reverse . toRows $ m

-- | reverse columns
fliprl :: (Storable t, Trans t) => Matrix t -> Matrix t
fliprl m = fromCols . reverse . toCols $ m   

{- | display with n digits after the decimal point.
-}
disp :: (Disp a) => Int -> a -> IO ()
disp n = putStrLn . format n

--------------------------------------------

-- | sum of columns of a matrix.
sumCols :: (Mul V (Matrix t) (Vector t)) => Matrix t -> Vector t
sumCols m = constant 1 (rows m) <> m

{- | outer product of two vectors.

@\> realVector [1,2,3] \`outer\` complexVector [7,0,2*i,1+i]
 7.  0.  2.i  1.+1.i
14.  0.  4.i  2.+2.i
21.  0.  6.i  3.+3.i@

-}
outer :: (Mul (Matrix a) (Matrix b) (Matrix r)) => Vector a -> Vector b -> Matrix r
outer u v = reshape 1 u <> reshape (size v) v


    
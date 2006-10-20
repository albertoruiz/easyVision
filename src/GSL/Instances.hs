{-# OPTIONS #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSL.Instances
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses -fffi and -fglasgow-exts

Creates reasonable numeric instances for Vectors and Matrices. In the context of the standard numeric operators, one-component vectors and matrices automatically expand to match the dimensions of the other operand.

-}
-----------------------------------------------------------------------------
-- #hide

module GSL.Instances where

import GSL.Types
import GSL.Matrix
import GSL.Wrappers
import GSL.Common
import Complex

adaptScalar f1 f2 f3 x y 
    | size x == 1 = f1 (x@>0) y
    | size y == 1 = f3 x (y@>0)
    | otherwise = f2 x y

---------------------------------------------------

instance Eq (Vector Double) where
    u == v = toList u == toList v


subvv = vectorZip 4
subvc v c = addConstant (-c) v
subcv c v = addConstant c (scale (-1) v)

mul = vectorZip 1

instance Num (Vector Double) where
    (+) = adaptScalar addConstant add (flip addConstant)
    (-) = adaptScalar subcv subvv subvc
    (*) = adaptScalar scale mul (flip scale)
    abs = vectorMap 3
    signum = vectorMap 15
    fromInteger n = fromList [fromInteger n]

----------------------------------------------------

instance Eq (Vector (Complex Double)) where
    u == v = toList u == toList v

addConstantC a = gmap (+a)
subCvv u v = u `add` scale (-1) v
subCvc v c = addConstantC (-c) v
subCcv c v = addConstantC c (scale (-1) v)


instance Num (Vector (Complex Double)) where
    (+) = adaptScalar addConstantC add (flip addConstantC)
    (-) = adaptScalar subCcv subCvv subCvc
    (*) = adaptScalar scale (gzip (*)) (flip scale)
    abs = gmap abs
    signum = gmap signum
    fromInteger n = fromList [fromInteger n]


-- | adapts a function on two vectors to work on all the toList of two matrices
asVector2' :: (Vector a -> Vector b -> Vector c) -> Matrix a -> Matrix b -> Matrix c
asVector2' f m1@(M r1 c1 _) m2@(M r2 c2 _)
    | sameShape m1 m2 || r1*c1==1 || r2*c2==1
        = reshape (max c1 c2) $ f (flatten m1) (flatten m2)
    | otherwise = error "inconsistent matrix dimensions" 

---------------------------------------------------

instance Eq (Matrix Double) where
    u == v = toLists u == toLists v

instance Num (Matrix Double) where
    (+) = asVector2' (+)
    (-) = asVector2' (-)
    (*) = asVector2' (*)
    abs = asVector abs
    signum = asVector signum
    fromInteger n = fromLists [[fromInteger n]]

----------------------------------------------------

instance Eq (Matrix (Complex Double)) where
    u == v = toLists u == toLists v

instance Num (Matrix (Complex Double)) where
    (+) = asVector2' (+)
    (-) = asVector2' (-)
    (*) = asVector2' (*)
    abs = asVector abs
    signum = asVector signum
    fromInteger n = fromLists [[fromInteger n]]

------------------------------------------------------

instance Fractional (Vector Double) where
    fromRational n = fromList [fromRational n]
    (/) = adaptScalar f (vectorZip 2) g where
        r `f` v = vectorZip 2 (constant r (size v)) v
        v `g` r = scale (recip r) v

-------------------------------------------------------

instance Fractional (Vector (Complex Double)) where
    fromRational n = fromList [fromRational n]
    (/) = adaptScalar f (gzip (/)) g where
        r `f` v = gmap ((*r).recip) v
        v `g` r = gmap (/r) v

------------------------------------------------------

instance Fractional (Matrix Double) where
    fromRational n = fromLists [[fromRational n]]
    (/) = asVector2' (/)

-------------------------------------------------------

instance Fractional (Matrix (Complex Double)) where
    fromRational n = fromLists [[fromRational n]]
    (/) = asVector2' (/)

---------------------------------------------------------

instance Floating (Vector Double) where
    sin   = vectorMap 0
    cos   = vectorMap 1
    tan   = vectorMap 2
    asin  = vectorMap 4
    acos  = vectorMap 5
    atan  = vectorMap 6
    sinh  = vectorMap 7
    cosh  = vectorMap 8
    tanh  = vectorMap 9
    asinh = vectorMap 10
    acosh = vectorMap 11
    atanh = vectorMap 12
    exp   = vectorMap 13
    log   = vectorMap 14  
    pi    = fromList [pi]

-----------------------------------------------------------

instance Floating (Matrix Double) where
    sin   = asVector sin
    cos   = asVector cos
    tan   = asVector tan
    asin  = asVector asin
    acos  = asVector acos
    atan  = asVector atan
    sinh  = asVector sinh
    cosh  = asVector cosh
    tanh  = asVector tanh
    asinh = asVector asinh
    acosh = asVector acosh
    atanh = asVector atanh
    exp   = asVector exp
    log   = asVector log  
    pi    = fromLists [[pi]]

-------------------------------------------------------------

instance Floating (Vector (Complex Double)) where
    sin   = gmap sin
    cos   = gmap cos
    tan   = gmap tan
    asin  = gmap asin
    acos  = gmap acos
    atan  = gmap atan
    sinh  = gmap sinh
    cosh  = gmap cosh
    tanh  = gmap tanh
    asinh = gmap asinh
    acosh = gmap acosh
    atanh = gmap atanh
    exp   = gmap exp
    log   = gmap log
    pi    = fromList [pi]

---------------------------------------------------------------

instance Floating (Matrix (Complex Double)) where
    sin   = asVector sin
    cos   = asVector cos
    tan   = asVector tan
    asin  = asVector asin
    acos  = asVector acos
    atan  = asVector atan
    sinh  = asVector sinh
    cosh  = asVector cosh
    tanh  = asVector tanh
    asinh = asVector asinh
    acosh = asVector acosh
    atanh = asVector atanh
    exp   = asVector exp
    log   = asVector log  
    pi    = fromLists [[pi]]

---------------------------------------------------------------

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


instance Mul Double Double Double where
 (<>) = (*)
  
instance Mul Double (Complex Double) (Complex Double) where
 a <> b = (a:+0) * b
   
instance Mul (Complex Double) Double (Complex Double) where
 a <> b = a * (b:+0)
    
instance Mul (Complex Double) (Complex Double) (Complex Double) where
 (<>) = (*)

--------------------------------- matrix matrix

instance Mul RMatrix RMatrix RMatrix where
 (<>) = mXm

instance Mul CMatrix CMatrix CMatrix where
 (<>) = mXm

instance Mul CMatrix RMatrix CMatrix where
 c <> r = c <> complex r

instance Mul RMatrix CMatrix CMatrix where
 r <> c = complex r <> c

--------------------------------- RMatrix RVector

instance Mul RMatrix RVector RVector where
 (<>) = mXv

instance Mul CMatrix CVector CVector where
 (<>) = mXv

instance Mul CMatrix RVector CVector where
 m <> v = m <> complex v

instance Mul RMatrix CVector CVector where
 m <> v = complex m <> v

--------------------------------- RVector RMatrix

instance Mul RVector RMatrix RVector where
 (<>) = vXm
 
instance Mul CVector CMatrix CVector where
 (<>) = vXm
 
instance Mul CVector RMatrix CVector where
 v <> m = v <> complex m
 
instance Mul RVector CMatrix CVector where
 v <> m = complex v <> m

--------------------------------- dot product

instance Mul RVector RVector Double where
 (<>) = dot
 
instance Mul CVector CVector (Complex Double) where
 (<>) = dot
  
instance Mul RVector CVector (Complex Double) where
 a <> b = complex a <> b
 
instance Mul CVector RVector (Complex Double) where
 (<>) = flip (<>)
 
--------------------------------- scaling vectors  
  
instance Mul Double RVector RVector where
 (<>) = scale

instance Mul RVector Double RVector where
 (<>) = flip (<>)
  
instance Mul (Complex Double) CVector CVector where
 (<>) = scaleC

instance Mul CVector (Complex Double) CVector where
 (<>) = flip (<>)

instance Mul Double CVector CVector where
 a <> v = (a:+0) <> v

instance Mul CVector Double CVector where
 (<>) = flip (<>)

instance Mul (Complex Double) RVector CVector where
 a <> v = a <> complex v

instance Mul RVector (Complex Double) CVector where
 (<>) = flip (<>)

--------------------------------- scaling matrices

instance Mul Double RMatrix RMatrix where
 (<>) a = asVector (a <>)

instance Mul RMatrix Double RMatrix where
 (<>) = flip (<>)

instance Mul (Complex Double) CMatrix CMatrix where
 (<>) a = asVector (a <>)

instance Mul CMatrix (Complex Double) CMatrix where
 (<>) = flip (<>)

instance Mul Double CMatrix CMatrix where
 a <> m = (a:+0) <> m

instance Mul CMatrix Double CMatrix where
 (<>) = flip (<>)

instance Mul (Complex Double) RMatrix CMatrix where
 a <> m = a <> complex m

instance Mul RMatrix (Complex Double) CMatrix where
 (<>) = flip (<>)

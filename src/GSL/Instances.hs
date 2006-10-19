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

module GSL.Instances where

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
subvc c = addConstant (-c)
subcv v c = addConstant c (scale (-1) v)

mul = vectorZip 1

instance Num (Vector Double) where
    (+) = adaptScalar addConstant add (flip addConstant)
    (-) = adaptScalar subvc subvv subcv
    (*) = adaptScalar scale mul (flip scale)
    abs = vectorMap 3
    signum = vectorMap 15
    fromInteger n = fromList [fromInteger n]

----------------------------------------------------

instance Eq (Vector (Complex Double)) where
    u == v = toList u == toList v

addConstantC a = gmap (+a)
subCvv u v = u `add` scale (-1) v
subCvc c = addConstantC (-c)
subCcv v c = addConstantC c (scale (-1) v)


instance Num (Vector (Complex Double)) where
    (+) = adaptScalar addConstantC add (flip addConstantC)
    (-) = adaptScalar subCvc subCvv subCcv
    (*) = adaptScalar scale (gzip (*)) (flip scale)
    abs = gmap abs
    signum = gmap signum
    fromInteger n = fromList [fromInteger n]


---------------------------------------------------

instance Eq (Matrix Double) where
    u == v = toLists u == toLists v

instance Num (Matrix Double) where
    (+) = asVector2 (+)
    (-) = asVector2 (-)
    (*) = asVector2 (*)
    abs = asVector abs
    signum = asVector signum
    fromInteger n = fromLists [[fromInteger n]]

----------------------------------------------------

instance Eq (Matrix (Complex Double)) where
    u == v = toLists u == toLists v

instance Num (Matrix (Complex Double)) where
    (+) = asVector2 (+)
    (-) = asVector2 (-)
    (*) = asVector2 (*)
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
    (/) = asVector2 (/)

-------------------------------------------------------

instance Fractional (Matrix (Complex Double)) where
    fromRational n = fromLists [[fromRational n]]
    (/) = asVector2 (/)

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

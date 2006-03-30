{-# OPTIONS -fglasgow-exts #-}
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

import GSL.Core
import GSL.Wrappers
import GSL.Derived
import GSL.Interface
import Foreign
import Complex

adaptScalar f1 f2 f3 x y 
    | size x == 1 = f1 (x!:0) y
    | size y == 1 = f3 x (y!:0)
    | otherwise = f2 x y

---------------------------------------------------

instance Eq Vector where
    u == v = toList u == toList v

instance Num Vector where
    (+) = adaptScalar (|+|) (|+|) (|+|)
    (-) = adaptScalar (|-|) (vectorZip 4) (|-|)
    (*) = adaptScalar (<>) (.*) (<>)
    abs = vectorMap 3
    signum = vectorMap 15
    fromInteger n = fromList [fromInteger n]
    
----------------------------------------------------

instance Eq ComplexVector where
    u == v = toList u == toList v    

instance Num ComplexVector where
    (+) = adaptScalar (|+|) (|+|) (|+|)
    (-) = adaptScalar (|-|) (|-|) (|-|)
    (*) = adaptScalar (<>) (.*) (<>)
    abs = vmap abs
    signum = vmap signum
    fromInteger n = fromList [fromInteger n]
    
---------------------------------------------------

adaptScalar' f1 f2 f3 x y 
    | size (flatten x) == 1 = f1 (x!!:(0,0)) y
    | size (flatten y) == 1 = f3 x (y!!:(0,0))
    | otherwise = f2 x y

---------------------------------------------------

instance Eq Matrix where
    u == v = toLists u == toLists v

instance Num Matrix where
    (+) = adaptScalar' (|+|) (|+|) (|+|)
    (-) = adaptScalar' (|-|) (asVector2 (vectorZip 4)) (|-|)
    (*) = adaptScalar' (<>) (.*) (<>)
    abs = asVector (vectorMap 3)
    signum = asVector (vectorMap 15)
    fromInteger n = fromLists [[fromInteger n]]
    
----------------------------------------------------

instance Eq ComplexMatrix where
    u == v = toLists u == toLists v    

instance Num ComplexMatrix where
    (+) = adaptScalar' (|+|) (|+|) (|+|)
    (-) = adaptScalar' (|-|) (|-|) (|-|)
    (*) = adaptScalar' (<>) (.*) (<>)
    abs = mmap abs
    signum = mmap signum
    fromInteger n = fromLists [[fromInteger n]]
    
------------------------------------------------------

instance Fractional Vector where
    fromRational n = fromList [fromRational n]
    (/) = adaptScalar f (vectorZip 2) g where
        r `f` v = vectorZip 2 (constant r v) v
        v `g` r = v <> recip r
        
-------------------------------------------------------
        
instance Fractional ComplexVector where
    fromRational n = fromList [fromRational n]
    (/) = adaptScalar f (vzip (/)) g where
        r `f` v = vmap ((*r).recip) v
        v `g` r = vmap (/r) v
        
------------------------------------------------------

instance Fractional Matrix where
    fromRational n = fromLists [[fromRational n]]
    (/) = adaptScalar' f (asVector2 (vectorZip 2)) g where
        r `f` m = asVector2 (vectorZip 2) (constant r m) m
        m `g` r = m <> recip r
        
-------------------------------------------------------
        
instance Fractional ComplexMatrix where
    fromRational n = fromLists [[fromRational n]]
    (/) = adaptScalar' f (mzip (/)) g where
        r `f` m = mmap ((*r).recip) m
        m `g` r = mmap (/r) m
        
---------------------------------------------------------
        
instance Floating Vector where
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
    
instance Floating Matrix where
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
    
instance Floating ComplexVector where
    sin   = vmap sin
    cos   = vmap cos
    tan   = vmap tan
    asin  = vmap asin
    acos  = vmap acos
    atan  = vmap atan
    sinh  = vmap sinh
    cosh  = vmap cosh
    tanh  = vmap tanh
    asinh = vmap asinh
    acosh = vmap acosh
    atanh = vmap atanh
    exp   = vmap exp
    log   = vmap log  
    pi    = fromList [pi]     
       
---------------------------------------------------------------       

instance Floating ComplexMatrix where
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

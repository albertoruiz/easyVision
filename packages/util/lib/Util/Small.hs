{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
{- |
Module      :  Util.Small
Copyright   :  (c) Alberto Ruiz 2012
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Type safe small arrays.

-}
-----------------------------------------------------------------------------

module Util.Small
(   Shaped(..),
#if __GLASGOW_HASKELL__ < 704
    Array, DArray,
#endif
    Dim2(..),Dim3(..),Dim4(..), Build, (!), (#),
    Dim2x2,Dim3x3,Dim4x4,Dim3x4,
    vec2,vec3,vec4,
    Vectorlike(..), Matrixlike(..)
--    , v,m, KK(..)
) where

import Numeric.LinearAlgebra(Vector,(@>),(><),fromRows,toRows,toList,flatten)
import Foreign.Storable
import Foreign.Ptr
import Util.Misc(Mat,Vec)
import Data.Packed.ST(runSTVector,newUndefinedVector,writeVector)

--------------------------------------------------------------------------------

vec2 :: Double -> Double -> Vector Double
vec2 a b = runSTVector $ do
    v <- newUndefinedVector 2
    writeVector v 0 a
    writeVector v 1 b
    return v

vec3 :: Double -> Double -> Double -> Vector Double
vec3 a b c = runSTVector $ do
    v <- newUndefinedVector 3
    writeVector v 0 a
    writeVector v 1 b
    writeVector v 2 c
    return v


vec4 :: Double -> Double -> Double -> Double -> Vector Double
vec4 a b c d = runSTVector $ do
    v <- newUndefinedVector 4
    writeVector v 0 a
    writeVector v 1 b
    writeVector v 2 c
    writeVector v 3 d
    return v

--------------------------------------------------------------------------------

data Dim2 a = D2 !a !a        deriving (Eq, Show, Read)
data Dim3 a = D3 !a !a !a     deriving (Eq, Show, Read)
data Dim4 a = D4 !a !a !a !a  deriving (Eq, Show, Read)


class Build p e g | e p -> g, g -> e, g -> p
  where
    build :: p -> e -> g

infixl 3 !
(!) :: Build p e g => p -> e -> g
(!) = build

infixl 2 #
(#) :: Build p e g => p -> e -> g
(#) = build

instance Build a a (Dim2 a) where
    build = D2

instance Build (Dim2 a) a (Dim3 a) where
    build (D2 x y) a = D3 x y a

instance Build (Dim3 a) a (Dim4 a) where
    build (D3 x y z) a = D4 x y z a

--------------------------------------------------------------------------------

type Dim2x2 = Dim2 (Dim2 Double)
type Dim3x3 = Dim3 (Dim3 Double)
type Dim3x4 = Dim3 (Dim4 Double)
type Dim4x4 = Dim4 (Dim4 Double)

--------------------------------------------------------------------------------

type family DArray (m :: *)

type instance DArray (Dim2 Double) = Vec
type instance DArray (Dim3 Double) = Vec
type instance DArray (Dim4 Double) = Vec
type instance DArray Dim2x2        = Mat
type instance DArray Dim3x4        = Mat
type instance DArray Dim3x3        = Mat
type instance DArray Dim4x4        = Mat

type family Array (m :: *)
type instance Array a = DArray (Shape a)

#
class Shaped a where
    type Shape a
    toArray         :: a -> Array a
    unsafeFromArray :: Array a -> a
    toDim           :: (Array a ~ Array (Shape a), Shaped (Shape a)) => a -> Shape a
    fromDim         :: (Array a ~ Array (Shape a), Shaped (Shape a)) => Shape a -> a
    toDim = unsafeFromArray . toArray
    fromDim = unsafeFromArray . toArray


instance Shaped (Dim2 Double) where
    type Shape (Dim2 Double) = Dim2 Double
    toArray (D2 x1 x2) = vec2 x1 x2
    unsafeFromArray v = D2 (v@>0) (v@>1)

instance Shaped (Dim3 Double) where
    type Shape (Dim3 Double) = Dim3 Double
    toArray (D3 x1 x2 x3) = vec3 x1 x2 x3
    unsafeFromArray v = D3 (v@>0) (v@>1) (v@>2)

instance Shaped (Dim4 Double) where
    type Shape (Dim4 Double) = Dim4 Double
    toArray (D4 x1 x2 x3 x4) = vec4 x1 x2 x3 x4
    unsafeFromArray v = D4 (v@>0) (v@>1) (v@>2) (v@>3)

instance Shaped Dim2x2 where
    type Shape Dim2x2 = Dim2x2
    toArray (D2 (D2 x1 x2)
                (D2 x3 x4) ) = (2><2) [x1,x2,
                                       x3,x4]
    unsafeFromArray m = (D2 (D2 x1 x2)
                            (D2 x3 x4) )
      where
        [x1,x2,x3,x4] = toList (flatten m)

instance Shaped Dim3x3 where
    type Shape Dim3x3 = Dim3x3
    toArray (D3 (D3 x1 x2 x3)
                (D3 x4 x5 x6)
                (D3 x7 x8 x9) ) = (3><3) [x1,x2,x3,
                                          x4,x5,x6,
                                          x7,x8,x9]
    unsafeFromArray m = (D3 (D3 x1 x2 x3)
                            (D3 x4 x5 x6)
                            (D3 x7 x8 x9) )
      where
        [x1,x2,x3,x4,x5,x6,x7,x8,x9] = toList (flatten m)

instance Shaped Dim3x4 where
    type Shape Dim3x4 = Dim3x4
    toArray (D3 r1 r2 r3) = fromRows (map toArray [r1,r2,r3])
    unsafeFromArray m = (D3 d1 d2 d3)
       where
         [d1,d2,d3] = map unsafeFromArray (toRows m)


instance Shaped Dim4x4 where
    type Shape Dim4x4 = Dim4x4
    toArray (D4 r1 r2 r3 r4) = fromRows (map toArray [r1,r2,r3,r4])
    unsafeFromArray m = (D4 d1 d2 d3 d4)
       where
         [d1,d2,d3,d4] = map unsafeFromArray (toRows m)

--------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ < 704
class Matrixlike x where
    toMatrix         :: (Shaped x, Array x ~ Mat) => x -> Mat
    unsafeFromMatrix :: (Shaped x, Array x ~ Mat) => Mat -> x
    toMatrix = toArray
    unsafeFromMatrix = unsafeFromArray

class Vectorlike x where
    toVector         :: (Shaped x, Array x ~ Vec) => x -> Vec
    unsafeFromVector :: (Shaped x, Array x ~ Vec) => Vec -> x
    toVector = toArray
    unsafeFromVector = unsafeFromArray
    datMat :: (Shaped x, DArray (Shape x) ~ Vec) => [x] -> Mat
    datMat = fromRows . map toVector
    unsafeMatDat :: (Shaped x, DArray (Shape x) ~ Vec) => Mat -> [x]
    unsafeMatDat = map unsafeFromVector . toRows
#else
class (Shaped x, Array x ~ Mat) => Matrixlike x where
    toMatrix         :: x -> Mat
    unsafeFromMatrix :: Mat -> x
    toMatrix = toArray
    unsafeFromMatrix = unsafeFromArray

class (Shaped x, Array x ~ Vec) => Vectorlike x where
    toVector         :: x -> Vec
    unsafeFromVector :: Vec -> x
    toVector = toArray
    unsafeFromVector = unsafeFromArray
    datMat :: [x] -> Mat
    datMat = fromRows . map toVector
    unsafeMatDat :: Mat -> [x]
    unsafeMatDat = map unsafeFromVector . toRows
#endif

instance (Shaped x, Array x ~ Mat) => Matrixlike x

instance (Shaped x, Array x ~ Vec) => Vectorlike x

--------------------------------------------------------------------------------

-- copied from storable-complex

instance Storable t => Storable (Dim2 t) where
    sizeOf _ = 2*sizeOf (undefined :: t)
    alignment _ = alignment (undefined :: t)
    peek p = do
        let q = castPtr p
        a <- peek q
        b <- peekElemOff q 1
        return (D2 a b)
    poke p (D2 a b) = do
        let q = (castPtr p)
        poke q a
        pokeElemOff q 1 b

instance Storable t => Storable (Dim3 t) where
    sizeOf _ = 3*sizeOf (undefined :: t)
    alignment _ = alignment (undefined :: t)
    peek p = do
        let q = castPtr p
        a <- peek q
        b <- peekElemOff q 1
        c <- peekElemOff q 2
        return (D3 a b c)
    poke p (D3 a b c) = do
        let q = (castPtr p)
        poke q a
        pokeElemOff q 1 b
        pokeElemOff q 2 c

instance Storable t => Storable (Dim4 t) where
    sizeOf _ = 4*sizeOf (undefined :: t)
    alignment _ = alignment (undefined :: t)
    peek p = do
        let q = castPtr p
        a <- peek q
        b <- peekElemOff q 1
        c <- peekElemOff q 2
        d <- peekElemOff q 3
        return (D4 a b c d)
    poke p (D4 a b c d) = do
        let q = (castPtr p)
        poke q a
        pokeElemOff q 1 b
        pokeElemOff q 2 c
        pokeElemOff q 3 d


instance (Array x ~ Array (Shape x),
          Shaped x, 
          Shaped (Shape x),
          Storable (Shape x)) => Storable x where
    sizeOf = sizeOf . toDim
    alignment = alignment . toDim
    peek = fmap fromDim . peek . castPtr
    poke p = poke (castPtr p) . toDim

--------------------------------------------------------------------------------
 -- tests
{-

v = 2 ! 3 :: Dim2 Double

m = 1 ! 2 ! 3
  # 3 ! 4 ! 0
  # 5 ! 6 ! 0 :: Dim3x3

data KK = KK Double Double deriving Show

instance Shaped KK where
    type Shape KK = Dim2 Double
    toArray (KK x1 x2) = vec2 x1 x2
    unsafeFromArray v = KK (v@>0) (v@>1)
-}


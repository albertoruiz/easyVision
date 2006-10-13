{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  GSL.LinearAlgebra.Unboxed
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Some linear algebra algorithms working on UArray.

(In construction...)

-}
-----------------------------------------------------------------------------
module GSL.LinearAlgebra.Unboxed (
    mulR,
    mulC,
    eigS,
    eigH,
) where

import Foreign
import Data.Array.Storable(StorableArray)
import qualified GSL.LinearAlgebra.Storable as S
import Data.Array.MArray

import GSL.LinearAlgebra.Matrix


adapt2_1 op x y = unsafePerformIO $ do
    x' <- unsafeThaw x
    y' <- unsafeThaw y
    z' <- op x' y'
    z  <- unsafeFreeze z'
    return z

adapt1_2 op x = unsafePerformIO $ do
    x' <- unsafeThaw x
    (a',b') <- op x'
    a <- unsafeFreeze a'
    b <- unsafeFreeze b'
    return (a,b)

------------------------------------------------------------------

mulR :: UMatrix -> UMatrix -> UMatrix 
mulR = adapt2_1 S.mulR

------------------------------------------------------------------

mulC :: UCMatrix -> UCMatrix  -> UCMatrix
mulC = adapt2_1 S.mulC

------------------------------------------------------------------

eigS :: UMatrix -> (UVector, UMatrix)
eigS = adapt1_2 S.eigS

------------------------------------------------------------------

eigH:: UCMatrix -> (UVector, UCMatrix)
eigH = adapt1_2 S.eigH

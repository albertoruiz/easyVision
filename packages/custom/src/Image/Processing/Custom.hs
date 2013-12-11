{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
{- |
Module      :  Image.Processing.Custom
Copyright   :  (c) Alberto Ruiz 2013
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module Image.Processing.Custom (
    histogram3D
)
where

import Image.Devel
import Foreign.C.Types ( CInt(CInt) )
import Data.Packed.Vector(Vector)
import Data.Packed.Development(createVector)
import Data.Packed.Foreign(appVectorLen)

--------------------------------------------------------------------------------

foreign import ccall unsafe "histogram3D"
    c_histogram3D :: CInt -> CInt -> RawImage I8u3 (CInt -> Ptr Float -> IO CInt)

histogram3D :: Int -> Image I8u3 -> Vector Float
histogram3D b x = unsafePerformIO $ do
    y <- createVector (n*n*n)
    withImage x $ checkFFI "histogram3D" $ do
        c_histogram3D (fi n) (fi d) `appI` x `appVectorLen` y
    return y
  where
    n = 2^b
    d = 8-b

--------------------------------------------------------------------------------


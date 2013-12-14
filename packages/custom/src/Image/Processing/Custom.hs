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
    histogram3D,
    getPoints8u,
    module Image.Processing.Simple
)
where

import Image.Devel
import Foreign.C.Types ( CInt(CInt) )
import Data.Packed.Vector(Vector)
import Data.Packed.Development(createVector)
import Data.Packed.Foreign(appVectorLen)
import Image.Processing.Simple

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

getPoints8u :: Image I8u -> [Pixel]
getPoints8u x = go r1 c1 []
  where
    ROI r1 r2 c1 c2 = roi x
    go r c ps | r > r2                      =                ps
              | c > c2                      = go (r+1) c1    ps
              | readPixel x (Pixel r c) > 0 = go r     (c+1) (Pixel r c:ps)
              | otherwise                   = go r     (c+1) ps

--------------------------------------------------------------------------------


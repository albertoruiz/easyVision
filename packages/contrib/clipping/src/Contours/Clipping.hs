{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
{- |
Module      :  Contours.Clipping
Copyright   :  (c) PARP Research Group, University of Murcia, 2012
License     :  GPL
Maintainer  :  Alberto Ruiz
Stability   :  provisional

Haskell interface to efficient clipping of arbitrary polygons.

Implementation in C by Adrián Amor Martínez.

Algorithm by G. Greiner, K. Hormann, ACM Transactions on Graphics.

-}
-----------------------------------------------------------------------------

module Contours.Clipping (
    clip, ClipMode(..)
)
where

import ImagProc.Base
import ImagProc.Ipp.Core
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import System.IO.Unsafe(unsafePerformIO)
import Control.Applicative((<$>))
import Data.Packed.Vector(takesV,fromList,toList)
import Contours.Base(orientedArea)

foreign import ccall "clip" c_clip
    :: Ptr Double -> Ptr Double -> CInt
    -> Ptr Double -> Ptr Double -> CInt
    -> Ptr (Ptr Double) -> Ptr (Ptr Double) -> Ptr (Ptr (CInt)) -> Ptr (CInt) -> CInt
    -> IO CInt

fixOrientation :: [Point] -> [Point]
fixOrientation xs = if orientedArea (Closed xs) > 0
                      then reverse xs
                      else xs

data ClipMode = ClipUnion
              | ClipIntersection
              | ClipDifference
              deriving (Enum, Show)

clip :: ClipMode -> Polyline -> Polyline -> [Polyline]
-- ^ compute the intersection of two polygons
clip mode (Closed a'') (Closed b'') = unsafePerformIO $ do
    ppxs <- malloc
    ppys <- malloc
    ppl  <- malloc
    pn   <- malloc

    let a' = fixOrientation a''
        b' = fixOrientation b''
        a = a' ++ [head a']
        b = b' ++ [head b']   
        nc = length a
        ns = length b

    cx <- newArray (map px a)
    cy <- newArray (map py a)
    sx <- newArray (map px b)
    sy <- newArray (map py b)
    
    --peekArray nc cx >>= print
    --peekArray nc cy >>= print
    --peekArray ns sx >>= print
    --peekArray ns sy >>= print

    _ok <- c_clip cx cy (fi nc) sx sy (fi ns) ppxs ppys ppl pn (2^fromEnum mode)

    --print _ok

    n <- ti <$> peek pn
    --print n
    
    pl <- peek ppl
    ls <- map ti <$> peekArray n pl
    --print ls

    let tot = sum ls
 
    pxs <- peek ppxs
    pys <- peek ppys

    xs <- peekArray tot pxs
    ys <- peekArray tot pys

    --mapM_ print $ zipWith (,) xs ys

    -- provisional
    let vxs = map (tail . toList) $ takesV ls (fromList xs)
        vys = map (tail . toList) $ takesV ls (fromList ys)
        r | n > 0 = zipWith f vxs vys
          | otherwise = []
          where f as bs = Closed $ zipWith Point as bs
    
    free pxs
    free pys
    free pl

    free ppxs
    free ppys
    free ppl
    free pn
    return r

clip _ _ _ = error "clip on open polylines not defined"


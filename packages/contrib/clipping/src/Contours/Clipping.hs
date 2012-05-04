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
    clip, ClipMode(..),
    xorext
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
    -> Ptr (Ptr Double) -> Ptr (Ptr Double) -> Ptr (Ptr CInt) -> Ptr (Ptr (CInt)) -> Ptr (CInt) -> CInt
    -> IO CInt

--------------------------------------------------------------------------------

data ClipMode = ClipUnion
              | ClipIntersection
              | ClipDifference
              | ClipXOR
              deriving (Enum, Show)

clip :: ClipMode -> Polyline -> Polyline -> [Polyline]
-- ^ set operations for polygons
clip m a b = map fst (preclip m b a)

--------------------------------------------------------------------------------

xorext :: Polyline -> Polyline -> [(Polyline, [Int])]
xorext = preclip ClipXOR

--------------------------------------------------------------------------------

fixOrientation :: Double -> [Point] -> [Point]
fixOrientation s xs = if signum (orientedArea (Closed xs)) /= signum s
                        then reverse xs
                        else xs

--------------------------------------------------------------------------------

preclip :: ClipMode -> Polyline -> Polyline -> [(Polyline, [Int])]
-- ^ interface to C function to compute set operation and origin for each vertex
preclip mode (Closed a'') (Closed b'') = unsafePerformIO $ do
    ppxs <- malloc
    ppys <- malloc
    ppos <- malloc
    ppl  <- malloc
    pn   <- malloc

    let a' = fixOrientation (-1) a''
        b' = fixOrientation   1  b''
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

    _ok <- c_clip cx cy (fi nc) sx sy (fi ns) ppxs ppys ppos ppl pn (2^fromEnum mode)

    --print _ok

    n <- ti <$> peek pn
    --print n
    
    pl <- peek ppl
    ls <- map ti <$> peekArray n pl
    --print ls

    let tot = sum ls
 
    pxs <- peek ppxs
    pys <- peek ppys
    pos <- peek ppos

    xs <- peekArray tot pxs
    ys <- peekArray tot pys
    os <- map ti `fmap` peekArray tot pos

    -- mapM_ print $ zip3 xs ys os

    -- provisional
    let vxs = map (init . toList) $ takesV ls (fromList xs)
        vys = map (init . toList) $ takesV ls (fromList ys)
        vos = map (init . toList) $ takesV ls (fromList os)
        r | n > 0 = zip (zipWith f vxs vys) vos
          | otherwise = []
          where f as bs = Closed $ zipWith Point as bs
    
    free pxs
    free pys
    free pos
    free pl

    free ppxs
    free ppys
    free ppos
    free ppl
    free pn
    return r

preclip _ _ _ = error "clip on open polylines not defined"


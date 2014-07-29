{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -O0 #-}

-----------------------------------------------------------------------------
{- |
Module      :  Contours.ClipRaw
Copyright   :  (c) PARP Research Group, University of Murcia, 2012
License     :  GPL
Maintainer  :  Alberto Ruiz
Stability   :  provisional

Haskell interface to efficient clipping of arbitrary polygons.

Implementation in C by Adrián Amor Martínez.

Algorithm by G. Greiner, K. Hormann, ACM Transactions on Graphics.

Additional info and support for degenerate cases by Pedro E. López de Teruel.

-}
-----------------------------------------------------------------------------

module Contours.ClipRaw (
    ClipMode(..), preclip
)
where

import Util.Geometry
import Image.Devel
import Foreign.C.Types
--import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
--import System.IO.Unsafe(unsafePerformIO)
import Control.Applicative((<$>))
import Data.Packed.Vector(takesV,fromList,toList)

--------------------------------------------------------------------------------

foreign import ccall "clip" c_clip
    :: Ptr Double -> Ptr Double -> CInt              -- polygon A
    -> Ptr Double -> Ptr Double -> CInt              -- polygon B
    -> CInt                                          -- operation code
    -> Ptr (Ptr Double) -> Ptr (Ptr Double)          -- output vertices
    -> Ptr (Ptr CInt) -> Ptr (CInt) -> Ptr (CInt)    -- array of lengths, its size, # of positive
    -> Ptr (CInt)                                    -- inside code
    -> Ptr (Ptr CInt)                                -- vertex origins
    -> Ptr (Ptr CInt) -> Ptr (Ptr CInt)
    -> Ptr (Ptr CInt) -> Ptr (Ptr CInt)              -- indexes
    -> Ptr (Ptr Double) -> Ptr (Ptr Double)          -- alphas
    -> IO CInt

--------------------------------------------------------------------------------

data ClipMode = ClipIntersection
              | ClipUnion
              | ClipDifference
              | ClipXOR
              deriving (Enum, Show)

--------------------------------------------------------------------------------

preclip :: ClipMode -> Polyline -> Polyline -> (([(Polyline, [Int])],Int),Int,Int)
preclip mode (Closed a') (Closed b') = unsafePerformIO $ do
    ppxs <- malloc
    ppys <- malloc
    ppos <- malloc
    ppl  <- malloc
    pn   <- malloc
    pnp  <- malloc
    pins <- malloc
    
    ppInd0A <- malloc
    ppInd1A <- malloc
    ppInd0B <- malloc
    ppInd1B <- malloc
    ppAlphaA <- malloc
    ppAlphaB <- malloc

    let a = a' ++ [head a']
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

    _ok <- c_clip cx cy (fi nc) sx sy (fi ns) (2^fromEnum mode)
                  ppxs ppys ppl pn pnp pins
                  ppos
                  ppInd0A ppInd1A ppInd0B ppInd1B
                  ppAlphaA ppAlphaB
                  
    --print _ok

    n <- ti <$> peek pn
    --print n
    np <- ti <$> peek pnp
    --print np
    
    insideCode <- ti <$> peek pins
    --print insideCode
    
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

    [pInd0A,pInd1A,pInd0B,pInd1B] <- mapM peek [ppInd0A,ppInd1A,ppInd0B,ppInd1B]
    [pAlphaA,pAlphaB] <- mapM peek [ppAlphaA,ppAlphaB]
    
    [ind0A,ind1A,ind0B,ind1B] <- mapM (peekArray tot) [pInd0A,pInd1A,pInd0B,pInd1B]
    [alphaA,alphaB] <- mapM (peekArray tot) [pAlphaA,pAlphaB]

    -- print (ls,os)
    
    -- mapM_ print $ zip3 xs ys os

    -- provisional
    let vxs = map (init . toList) $ takesV ls (fromList xs)
        vys = map (init . toList) $ takesV ls (fromList ys)
        vos = map (init . toList) $ takesV ls (fromList os)
        r | n > 0 = (zip (zipWith f vxs vys) vos, np)
          | otherwise = ([],0)
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

    free pInd0A
    free pInd1A
    free pInd0B
    free pInd1B
    free pAlphaA
    free pAlphaB
    
    free ppInd0A
    free ppInd1A
    free ppInd0B
    free ppInd1B
    free ppAlphaA
    free ppAlphaB
    
    return (r, n, insideCode)
  where
    px (Point x _) = x
    py (Point _ y) = y



preclip _ _ _ = error "clip on open polylines not defined"


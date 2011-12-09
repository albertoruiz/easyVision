{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.C.NP
Copyright   :  (c) Pedro E. Lopez de Teruel and Alberto Ruiz 2011
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Interface to the New Paradigm.

-}
-----------------------------------------------------------------------------

module ImagProc.C.NP (   
    npbRaw, npbParse, toPixels, npb
)
where

import ImagProc.Ipp.Core
import ImagProc(set)
import Foreign
--import Control.Arrow((***))

--------------------------------------------------------------------------------

foreign import ccall unsafe "getContoursAlberto"
    c_npb :: Ptr () -> Ptr () -> CInt -> CInt            -- ptr1 ptr2 step rows 
          -> CInt -> CInt -> CInt -> CInt                -- c1 c2 r1 r2
          -> Ptr (Ptr CInt) -> Ptr (Ptr CInt)            -- ps cs
          -> Ptr CInt -> Ptr CInt                        -- np nc
          -> IO ()

--------------------------------------------------------------------------------

npbRaw :: ImageGray -> ImageGray -> ([CInt], [CInt])
npbRaw x1 x2 = unsafePerformIO $ do
  let G im1 = x1
      G im2 = x2
      v z = fi . z . vroi $ im2

  mapM_ ((flip (set 0)) x2) (invalidROIs x2) 
  
  ppp <- new nullPtr
  ppc <- new nullPtr
  pn <- new 0
  cn <- new 0

  c_npb (ptr im1) (ptr im2)
        (fi.step $ im2) (fi.height.isize $ im2)
        (v c1) (v c2) (v r1) (v r2)
        ppp ppc pn cn
      
  pp <- peek ppp
  pc <- peek ppc
  p <- peek pn >>= flip peekArray pp . ti
  c <- peek cn >>= flip peekArray pc . ti
  mapM_ free [pn,cn,pp,pc]
  mapM_ free [ppp,ppc]
  mapM_ (touchForeignPtr.fptr) [im1,im2]
  return (p,c)

--------------------------------------------------------------------------------

npbParse :: CInt -> ([CInt], [CInt]) -> ([[Pixel]], [[Pixel]])
npbParse lmin (pts,szs) = go szs pts [] []
  where
    go  [] _ cls ops = (cls,ops)
    go [_] ps cls ops = (toPixels ps:cls,ops)
    go (n:m:ns) ps cls ops
        | m > 0     = go (m:ns) bs ncls ops 
        | otherwise = go   ns   ds cls  nops
      where
        (as,bs) = splitAt ( 2*(ti n)) ps
        (cs,ds) = splitAt (-2*(ti m)) bs
        ncls = if n >= lmin then toPixels as : cls else cls
        nops = if n-m >= lmin then (toPixelsI (reverse cs) ++ toPixels as) :ops else ops

-------------------------------------------------------------------------------

toPixels :: [CInt] -> [Pixel]
toPixels (x:y:zs) = Pixel (1+ti y) (1+ti x) : toPixels zs
toPixels _ = []

toPixelsI :: [CInt] -> [Pixel]
toPixelsI (x:y:zs) = Pixel (1+ti x) (1+ti y) : toPixelsI zs
toPixelsI _ = []


npb :: Int -> ImageGray -> ImageGray -> ([[Pixel]], [[Pixel]])
npb lmin x1 x2 = npbParse (fi lmin) . npbRaw x1 $ x2

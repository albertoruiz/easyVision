{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
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
    npbRaw, npbParse, toPixels, npb,
    wnpcontours
)
where

import ImagProc.Ipp.Core
import ImagProc
import Foreign
import EasyVision.GUI.Parameters(autoParam,intParam)
import Features.Polyline(douglasPeuckerClosed)
import ImagProc.Util((.@.))
import Control.Monad(when)

--------------------------------------------------------------------------------

foreign import ccall unsafe "getContoursAlberto"
    c_npb :: Ptr () -> Ptr () -> CInt -> CInt            -- ptr1 ptr2 step rows 
          -> CInt -> CInt -> CInt -> CInt                -- c1 c2 r1 r2
          -> Ptr (Ptr CInt) -> Ptr (Ptr CInt)            -- ps cs
          -> Ptr CInt -> Ptr CInt                        -- np nc
          -> IO ()

foreign import ccall unsafe "getContours"
   c_npb2 :: Ptr () -> Ptr () -> CInt -> CInt            -- ptr1 ptr2 step rows 
          -> CInt -> CInt -> CInt -> CInt                -- c1 c2 r1 r2
          -> Ptr (Ptr CInt) -> Ptr (Ptr CInt)            -- ps cs
          -> Ptr CInt -> Ptr CInt                        -- np nc
          -> IO ()

--------------------------------------------------------------------------------

npbRaw :: Int -> ImageGray -> ImageGray -> ([CInt], [CInt])
npbRaw mode x1 x2 = unsafePerformIO $ do
  let G im1 = x1
      G im2 = x2
      v z = fi . z . vroi $ im2
      cfun = [c_npb, c_npb2]

  when (mode==0) $ mapM_ ((flip (set 0)) x2) (invalidROIs x2) 
  
  ppp <- new nullPtr
  ppc <- new nullPtr
  pn <- new 0
  cn <- new 0

  (cfun!!mode) (ptr im1) (ptr im2)
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


npb :: Int -> Int -> ImageGray -> ImageGray -> ([[Pixel]], [[Pixel]])
npb mode lmin x1 x2 = npbParse (fi lmin) . npbRaw mode x1 $ x2

--------------------------------------------------------------------------------

autoParam "NPParam" ""
    [ ("rad1","Int",intParam 1 0 10)
    , ("rad2","Int",intParam 10 0 30)
    , ("th","Int",intParam 15 0 100)
    , ("radMM", "Int", intParam 2 0 10)
    , ("minlen", "Int", intParam 50 0 200)
    , ("mode", "Int", intParam 1 0 1)]

wnpcontours :: IO ImageGray ->  IO (IO (ImageGray, [Polyline]))
wnpcontours = npcontours .@. winNPParam

---------------------------------------

npcontours NPParam{mode = 0, ..} x = ok
  where
    x' = filterBox8u rad1 rad1 x
    y = filterBox8u rad2 rad2 x        
    (cl,_) = npb 0 minlen x' y
    ok = map proc cl
    proc = Closed . pixelsToPoints (size x). douglasPeuckerClosed 1.5

---------------------------------------

npcontours NPParam{mode = 1, ..} x = ok
  where
    x' = filterBox8u rad1 rad1 x
    y =  filterBox8u rad2 rad2 x
    mn = filterMin8u radMM x
    mx = filterMax8u radMM x
    dif = sub8u 1 mx mn
    mask = compareC8u (fromIntegral th) IppCmpGreater dif
    z = copyMask8u  y mask
    (cl,_) = npb 1 minlen x' z
    ok = map proc cl
    proc = Closed . pixelsToPoints (size x). douglasPeuckerClosed 1.5

---------------------------------------



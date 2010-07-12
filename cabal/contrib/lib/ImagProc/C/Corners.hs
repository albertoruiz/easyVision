{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.C.Corners
Copyright   :  (c) Alberto Ruiz 2010
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional

Interface to the FAST + Shi-Tomasi corner extractor used in PTAM and similar systems.

-}
-----------------------------------------------------------------------------

module ImagProc.C.Corners (
    fastCorners, prufun, prucor
)
where

import ImagProc.Ipp.Core
import Foreign
import Util.Misc(splitEvery)
import Data.Function(on)
import Data.List(sortBy)

----------------------------------------------------



fastCorners :: ImageGray -> [InterestPoint]
fastCorners _ = []


----------------------------------------------------

foreign import ccall "prufun"
    c_prufun :: Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt -> IO (CInt)

prufun = app1G c_prufun

----------------------------------------------------

foreign import ccall "prucor"
    c_prucor :: Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt ->
                Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt ->
                Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt ->
                CInt -> CInt ->
                CInt -> Ptr CInt -> Ptr CInt ->
                IO (CInt)

prucor th1 th2 nmax x y = unsafePerformIO $ do
    ptot <- malloc
    php <- mallocArray (ti nmax*3)
    z <- modifyROI (shrink(2,2).const (theROI x)) `fmap` image (size x)
    app1G (app1G (app1G c_prucor x) y) z (fi th1) (fi th2) nmax ptot php
    tot <- peek ptot
    hp <- peekArray (3*ti tot) php
    free ptot
    free php
    let (G x') = x; (G y') = y; (G z') = z
    mapM_ (touchForeignPtr.fptr) [x',y',z']
    return (z,sortBy (compare `on` (negate.fst)) . map f . splitEvery 3 $ hp)
  where
    f [r,c,score] = (ti score, Pixel (ti r) (ti c))

----------------------------------------------------

app1G f (G im) =
    f (ptr im) (fi.step$im)
      (fi $ r1 $ vroi im) (fi $ r2 $ vroi im) (fi.c1.vroi $ im) (fi.c2.vroi$im)

app1F f (F im) =
    f (ptr im) (fi.step$im)
      (fi $ r1 $ vroi im) (fi $ r2 $ vroi im) (fi.c1.vroi $ im) (fi.c2.vroi$im)
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
    fastCorners, prufun, prucor, sumInC, invertInC
)
where

import ImagProc.Ipp.Core
import ImagProc.Generic(clone)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import System.IO.Unsafe(unsafePerformIO)
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

------------------------------------------------

foreign import ccall "customSum"
    c_customSum :: Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr (CInt) -> IO (CInt)

sumInC (G x) = unsafePerformIO $ do
    presult <- malloc
    ok <- app1G c_customSum (G x) presult
    result <- peek presult
    touchForeignPtr . fptr $ x
    return result 

------------------------------------------------

foreign import ccall "customInvert"
    c_customInvert :: Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt
                   -> Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt
                   -> IO (CInt)

invertInC (G x) = unsafePerformIO $ do
    G res <- clone (G x)
    ok <- app1G c_customInvert (G x) `app1G` G res
    mapM_ (touchForeignPtr . fptr)  [x,res]
    return (G res) 

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

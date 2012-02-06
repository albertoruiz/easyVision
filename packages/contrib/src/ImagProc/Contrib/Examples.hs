{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Contrib.Examples
Copyright   :  (c) Alberto Ruiz 2012
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module ImagProc.Contrib.Examples (
    sumInC, invertInC,
    app1F, app1G
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

----------------------------------------------------

foreign import ccall "customSum"
    c_customSum :: Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt
                -> Ptr (CInt)
                -> IO (CInt)

sumInC :: ImageGray -> Int
sumInC (G x) = ti . unsafePerformIO $ do
    presult <- malloc
    _ok <- app1G c_customSum (G x) presult
    result <- peek presult
    touchForeignPtr . fptr $ x
    return result 

------------------------------------------------

foreign import ccall "customInvert"
    c_customInvert :: Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt
                   -> Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt
                   -> IO (CInt)

invertInC :: ImageGray -> ImageGray
invertInC (G x) = unsafePerformIO $ do
    G res <- clone (G x)
    _ok <- app1G c_customInvert (G x) `app1G` G res
    mapM_ (touchForeignPtr . fptr)  [x,res]
    return (G res) 

----------------------------------------------------

type RawImage t = Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt -> t

app1G :: RawImage t -> ImageGray -> t
app1G f (G im) = f (ptr im) (fi.step $ im) (g r1) (g r2) (g c1) (g c2)
  where
    g x = (fi . x . vroi) im

app1F ::  RawImage t -> ImageFloat -> t
app1F f (F im) = f (ptr im) (fi.step $ im) (g r1) (g r2) (g c1) (g c2)
  where
    g x = (fi . x . vroi) im


{-# OPTIONS -fffi #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.C.Simple
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Interface to a few simple algorithms implemented in C.

-}
-----------------------------------------------------------------------------


module ImagProc.C.Simple where

import Foreign
import Foreign.C.Types

foreign import ccall "simple.h getPoints32f"
    c_getPoints32f :: Ptr Float -> Int -> Int -> Int -> Int -> Int ->
                      Int -> Ptr CInt -> Ptr CInt -> IO Int

foreign import ccall "simple.h lbp8u"
     lbp8u :: Int -> Ptr () -> Int -> Int -> Int -> Int -> Int -> Ptr CInt -> IO Int

foreign import ccall "simple.h hsvcodeTest"
     hsvcodeTest :: Int -> Int -> Int -> Ptr () -> Int -> Int -> Int -> Int -> Int -> IO Int
foreign import ccall "simple.h hsvcode"
     hsvcode :: Int -> Int -> Int -> Ptr () -> Int -> Ptr () -> Int -> Int -> Int -> Int -> Int -> IO Int

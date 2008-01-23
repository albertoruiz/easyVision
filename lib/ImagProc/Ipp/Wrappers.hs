{-# OPTIONS -fffi #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Ipp.Wrappers
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Raw access to some IPP functions. You can use them directly, with the help of "ImagProc.Ipp.Core", but it is probably easier to use the higher level interface supplied by "ImagProc.ImageProcessing".

-}
-----------------------------------------------------------------------------


module ImagProc.Ipp.Wrappers where

import Foreign
import Foreign.C.Types

foreign import ccall "auxIpp.h auxWarpPerspective_32f_C1R"
     warpPerspective32f :: Ptr() -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr() -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr Double -> Int ->
                           IO Int

foreign import ccall "auxIpp.h auxWarpPerspective_8u_C1R"
     warpPerspectiveGray :: Ptr() -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr() -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr Double -> Int ->
                           IO Int

foreign import ccall "auxIpp.h auxWarpPerspective_8u_C3R"
     warpPerspectiveRGB :: Ptr() -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr() -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr Double -> Int ->
                           IO Int


foreign import ccall "auxIpp.h ippGetStatusString" ippGetStatusString :: Int -> IO (Ptr CChar)

foreign import ccall "auxIpp.h getPoints32f"
    c_getPoints32f :: Ptr Float -> Int -> Int -> Int -> Int -> Int ->
                      Int -> Ptr Int -> Ptr Int -> IO Int

foreign import ccall "auxIpp.h auxResize_32f_C1R"
     c_resize32f :: Ptr() -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr() -> Int ->
                           Int -> Int -> Int -> Int ->
                           Int ->
                           IO Int

foreign import ccall "auxIpp.h auxResize_8u_C1R"
     c_resize8u :: Ptr() -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr() -> Int ->
                           Int -> Int -> Int -> Int ->
                           Int ->
                           IO Int

foreign import ccall "auxIpp.h auxResize_8u_C3R"
     c_resize8u3 :: Ptr() -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr() -> Int ->
                           Int -> Int -> Int -> Int ->
                           Int ->
                           IO Int

foreign import ccall "auxIpp.h auxDCTFwd_32f_C1R"
     auxDCTFwd_32f_C1R :: Ptr Float -> Int ->
                          Int -> Int -> Int -> Int ->
                          Ptr Float -> Int ->
                          Int -> Int -> Int -> Int ->
                          IO Int

foreign import ccall "auxIpp.h auxDCTInv_32f_C1R"
     auxDCTInv_32f_C1R :: Ptr Float -> Int ->
                          Int -> Int -> Int -> Int ->
                          Ptr Float -> Int ->
                          Int -> Int -> Int -> Int ->
                          IO Int


foreign import ccall "auxIpp.h lbp8u"
     lbp8u :: Int -> Ptr () -> Int -> Int -> Int -> Int -> Int -> Ptr Int -> IO Int

foreign import ccall "auxIpp.h hsvcodeTest"
     hsvcodeTest :: Int -> Int -> Int -> Ptr () -> Int -> Int -> Int -> Int -> Int -> IO Int
foreign import ccall "auxIpp.h hsvcode"
     hsvcode :: Int -> Int -> Int -> Ptr () -> Int -> Ptr () -> Int -> Int -> Int -> Int -> Int -> IO Int



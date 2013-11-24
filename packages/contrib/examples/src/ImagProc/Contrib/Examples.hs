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
    sumInC, invertInC
)
where

import Image.Core
import Foreign(Word8)
--import ImagProc.Generic(clone)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import System.IO.Unsafe(unsafePerformIO)

----------------------------------------------------

foreign import ccall "customSum"
    c_customSum :: Ptr CInt -> RawImage Word8 (IO CInt)

sumInC :: ImageGray -> Int
sumInC x = ti . unsafePerformIO $ do
    presult <- malloc
    withImage x $ do
        c_customSum presult `appI` x // checkFFI "sumInC"
    result <- peek presult
    free presult
    return result 

------------------------------------------------

foreign import ccall "customInvert"
    c_customInvert :: RawImage Word8 (RawImage Word8 (IO CInt))
invertInC :: ImageGray -> ImageGray
invertInC = wrap11 "invertInC" c_customInvert

----------------------------------------------------


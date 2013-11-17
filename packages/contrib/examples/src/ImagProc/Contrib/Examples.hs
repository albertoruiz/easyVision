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

import Image.Devel
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
    c_customSum :: Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> CInt
                -> Ptr CInt
                -> IO CInt

sumInC :: ImageGray -> Int
sumInC (G x) = ti . unsafePerformIO $ do
    presult <- malloc
    _ok <- appG c_customSum (G x) presult
    result <- peek presult
    free presult
    touchForeignPtr . fptr $ x
    return result 

------------------------------------------------

foreign import ccall "customInvert"
    c_customInvert :: Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> CInt
                   -> Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> CInt
                   -> IO CInt

invertInC :: ImageGray -> ImageGray
invertInC (G x) = unsafePerformIO $ do
    G res <- image $ size (G x)
    _ok <- appG c_customInvert (G x) `appG` G res
    mapM_ (touchForeignPtr . fptr)  [x,res]
    return (G res) 

----------------------------------------------------


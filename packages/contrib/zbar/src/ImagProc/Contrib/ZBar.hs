{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Contrib.ZBar
Copyright   :  (c) Alberto Ruiz 2012
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Simple interface to zbar barcode reader.

-}
-----------------------------------------------------------------------------

module ImagProc.Contrib.ZBar (
    zbar
)
where

import ImagProc.Ipp.Core
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import System.IO.Unsafe(unsafePerformIO)
import Util.Misc(splitEvery,impossible)

----------------------------------------------------

foreign import ccall "c_zbar"
    c_zbar :: Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt
           -> Ptr CInt -> Ptr CString
           -> IO CInt

zbar :: ImageGray -> [(String,String)]
zbar (G x) = unsafePerformIO $ do
    presult <- malloc
    pstrings <- mallocArray 100 :: IO (Ptr CString)
    _ok <- app1G c_zbar (G x) presult pstrings
    result <- peek presult
    css <- peekArray (ti result) pstrings
    ss <- mapM peekCString css
    free presult
    free pstrings
    touchForeignPtr . fptr $ x
    return (map tup $ splitEvery 2 ss)
  where
    tup [a,b] = (a,b)
    tup _ = impossible "zbar/tup"

----------------------------------------------------

type RawImage t = Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt -> t

app1G :: RawImage t -> ImageGray -> t
app1G f (G im) = f (ptr im) (fi.step $ im) (g r1) (g r2) (g c1) (g c2)
  where
    g x = (fi . x . vroi) im


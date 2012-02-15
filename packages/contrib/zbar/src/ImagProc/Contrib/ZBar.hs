{-# LANGUAGE RecordWildCards, ForeignFunctionInterface #-}
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
    zbar, Barcode(..)
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

import ImagProc.Contrib.ZBar.Structs

----------------------------------------------------

foreign import ccall "c_zbar"
    c_zbar :: Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt
           -> Ptr CInt -> Ptr TBarcode
           -> IO CInt

data Barcode = Barcode { bcType  :: String
                       , bcValue :: String
                       , bcROI   :: ROI
                       } deriving Show

zbar :: ImageGray -> [Barcode]
zbar (G x) = unsafePerformIO $ do
    presult <- malloc
    pstrings <- mallocArray 50 -- FIXME
    _ok <- app1G c_zbar (G x) presult pstrings
    result <- peek presult
    css <- peekArray (ti result) pstrings
    bs <- mapM mkBarcode css
    free presult
    free pstrings
    touchForeignPtr . fptr $ x
    return bs
  where
    mkBarcode TBarcode {..} = do
        t <- peekCString bcSymbolType
        v <- peekCString bcSymbolValue
        return $ Barcode t v (ROI (ti bbR1) (ti bbR2) (ti bbC1) (ti bbC2))

----------------------------------------------------

type RawImage t = Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt -> t

app1G :: RawImage t -> ImageGray -> t
app1G f (G im) = f (ptr im) (fi.step $ im) (g r1) (g r2) (g c1) (g c2)
  where
    g x = (fi . x . vroi) im


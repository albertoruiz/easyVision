{-# LANGUAGE ForeignFunctionInterface, CPP #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Contrib.ZBar.Structs
Copyright   :  PARP Research Group, University of Murcia, Spain, 2012
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Storable instances of ZBar structs.

-}
-----------------------------------------------------------------------------

#include "wrappers.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

module ImagProc.Contrib.ZBar.Structs (
    TBarcode(..)
) where

import Foreign
import Foreign.C.Types(CInt)
import Foreign.C.String

data TBarcode = TBarcode {
    bcSymbolType  :: CString,
    bcSymbolValue :: CString,
    bbR1,bbR2,bbC1,bbC2 :: CInt
  }

instance Storable TBarcode where
    sizeOf _ = #size TBarcode
    alignment _ = #alignment TBarcode
    peek ptr = do
        t  <- (#peek TBarcode, symbol_type) ptr
        v <- (#peek TBarcode, symbol_value) ptr
        r1 <- (#peek TBarcode, bbr1) ptr
        r2 <- (#peek TBarcode, bbr2) ptr
        c1 <- (#peek TBarcode, bbc1) ptr
        c2 <- (#peek TBarcode, bbc2) ptr
        return $ TBarcode t v r1 r2 c1 c2

    poke _ _ = error $ "TBarcode poke not needed/implemented"

{-

typedef struct {
    const char * symbol_type;
    const char * symbol_value;
    int bbr1, bbr2, bbc1, bbc2;  // bounding box
} TBarcode;

-}


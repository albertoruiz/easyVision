{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Ipp.Structs
Copyright   :  (c) Alberto Ruiz 2008
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Storable instances of some ipp structs.

-}
-----------------------------------------------------------------------------

#include <ipp.h>
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

module ImagProc.Ipp.Structs (
    IppiSize(..),
    IppiPoint(..),
    IppiRect(..),
    IppiConnectedComp(..),
    Mask(..), codeMask,
    IppCmp(..), codeCmp,
    AlgHint(..), codeAlgHint,
    IppiNorm(..), codeNorm,
    codeSegment,
    FFTNormalization(..), codeFFTFlag
) where

import Foreign
import Foreign.C.Types(CInt)


data IppiSize = IppiSize CInt CInt

instance Storable IppiSize where
    sizeOf _ = #size IppiSize
    alignment _ = #alignment IppiSize
    peek ptr = do
        w <- (#peek IppiSize, width)  ptr
        h <- (#peek IppiSize, height) ptr
        return (IppiSize h w)
    poke ptr (IppiSize h w) = do
        (#poke IppiSize, width)  ptr w
        (#poke IppiSize, height) ptr h


data IppiPoint = IppiPoint CInt CInt

instance Storable IppiPoint where
    sizeOf _ = #size IppiPoint
    alignment _ = #alignment IppiPoint
    peek ptr = do
        x <- (#peek IppiPoint, x) ptr
        y <- (#peek IppiPoint, y) ptr
        return (IppiPoint x y)
    poke ptr (IppiPoint x y) = do
        (#poke IppiPoint, x) ptr x
        (#poke IppiPoint, y) ptr y


data IppiRect = IppiRect CInt CInt CInt CInt

instance Storable IppiRect where
    sizeOf _ = #size IppiRect
    alignment _ = #alignment IppiRect
    peek ptr = do
        x <- (#peek IppiRect, x)      ptr
        y <- (#peek IppiRect, y)      ptr
        w <- (#peek IppiRect, width)  ptr
        h <- (#peek IppiRect, height) ptr
        return (IppiRect x y w h)
    poke ptr (IppiRect x y w h) = do
        (#poke IppiRect, x)      ptr x
        (#poke IppiRect, y)      ptr y
        (#poke IppiRect, width)  ptr w
        (#poke IppiRect, height) ptr h


data IppiConnectedComp = IppiConnectedComp Double Double Double Double IppiRect

instance Storable IppiConnectedComp where
    sizeOf _ = #size IppiConnectedComp
    alignment _ = #alignment IppiConnectedComp
    peek ptr = do
        area  <- (#peek IppiConnectedComp, area)      ptr
        value0 <- (#peek IppiConnectedComp, value[0]) ptr
        value1 <- (#peek IppiConnectedComp, value[1]) ptr
        value2 <- (#peek IppiConnectedComp, value[2]) ptr
        rect  <- (#peek IppiConnectedComp, rect)      ptr
        return (IppiConnectedComp area value0 value1 value2 rect)
    poke ptr (IppiConnectedComp area value0 value1 value2 rect) = do
        (#poke IppiConnectedComp, area) ptr area
        (#poke IppiConnectedComp, value[0]) ptr value0
        (#poke IppiConnectedComp, value[1]) ptr value1
        (#poke IppiConnectedComp, value[2]) ptr value2
        (#poke IppiConnectedComp, rect) ptr rect


data Mask = Mask3x3 | Mask5x5
codeMask Mask3x3 = 33
codeMask Mask5x5 = 55

data AlgHint = AlgHintNone | AlgHintFast | AlgHintAccurate
codeAlgHint AlgHintNone     = 0
codeAlgHint AlgHintFast     = 1
codeAlgHint AlgHintAccurate = 2

-- | Comparison options
data IppCmp = IppCmpLess | IppCmpLessEq | IppCmpEq | IppCmpGreaterEq | IppCmpGreater
codeCmp IppCmpLess      = 0
codeCmp IppCmpLessEq    = 1
codeCmp IppCmpEq        = 2
codeCmp IppCmpGreaterEq = 3
codeCmp IppCmpGreater   = 4

-- | Normalization options for the FFT
data FFTNormalization = DivFwdByN | DivInvByN | DivBySqrtN | NoDivByAny
codeFFTFlag DivFwdByN  = 1
codeFFTFlag DivInvByN  = 2
codeFFTFlag DivBySqrtN = 4
codeFFTFlag NoDivByAny = 8

-- | Norm types
data IppiNorm = IppiNormInf | IppiNormL1 | IppiNormL2 | IppiNormFM

codeNorm IppiNormInf = 0
codeNorm IppiNormL1  = 1
codeNorm IppiNormL2  = 2
codeNorm IppiNormFM  = 3

codeSegment "IPP_SEGMENT_QUEUE"    = 0x01
codeSegment "IPP_SEGMENT_DISTANCE" = 0x02
codeSegment "IPP_SEGMENT_BORDER_4" = 0x40
codeSegment "IPP_SEGMENT_BORDER_8" = 0x80

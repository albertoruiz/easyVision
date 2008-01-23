{-# OPTIONS -ffi #-}
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
    IppiRect(..),
    IppiConnectedComp(..)
) where

import Foreign
import Foreign.C.Types(CInt)

data IppiRect = IppiRect Int Int Int Int

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

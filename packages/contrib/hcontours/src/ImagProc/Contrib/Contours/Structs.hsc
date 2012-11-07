{-# LANGUAGE ForeignFunctionInterface, CPP #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Contrib.Contours.Structs
Copyright   :  PARP Research Group, University of Murcia, Spain, 2012
License     :  All rights reserved

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Storable instances of the TContours and TPointContour structs.

-}
-----------------------------------------------------------------------------

#include "struct.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

module ImagProc.Contrib.Contours.Structs (
    TContours(..)
) where

import Foreign
import Foreign.C.Types(CInt,CChar)


data TContours = TContours
    { x, y, u, v  :: Ptr Float
    , b           :: Ptr CChar
    , cs          :: Ptr CInt
    , ccs         :: Ptr CInt
    , pn          :: CInt
    , cn          :: CInt
    }

instance Storable TContours where
    sizeOf _ = #size TContours
    alignment _ = #alignment TContours
    peek ptr = do
        x'  <- (#peek TContours, x)  ptr
        y'  <- (#peek TContours, y)  ptr
        u'  <- (#peek TContours, u)  ptr
        v'  <- (#peek TContours, v)  ptr
        b'  <- (#peek TContours, b)  ptr
        cs' <- (#peek TContours, cs) ptr
        ccs'<- (#peek TContours, cs) ptr
        pn' <- (#peek TContours, pn) ptr
        cn' <- (#peek TContours, cn) ptr
        return $ TContours x' y' u' v' b' cs' ccs' pn' cn'
    poke _ _ = error $ "TContours poke not needed/implemented"

{-

typedef struct {
    /// Arrays for contours data (x coordinates).
    float *x;
    /// Arrays for contours data (y coordinates).
    float *y;
    /// Arrays for contours data (u coordinates).
    float *u;
    /// Arrays for contours data (v coordinates).
    float *v;
    /// Arrays for contours data (b threshold).
    char *b;
    /// Array of sizes for contours data.
    int *cs;
    /// Array of starting indexes for contours data.
    int *ccs;
    /// Total number of points of all contours.
    int pn;
    /// Total number of contours.
    int cn;
} TContours;

-}


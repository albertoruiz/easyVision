{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.C.Segments.Structs
Copyright   :  (c) Alberto Ruiz 2008
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  uses ffi

Storable instance of the CSegment struct

-}
-----------------------------------------------------------------------------

#include "mycvSegments.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

module ImagProc.C.Segments.Structs (
    CSegment(..)
) where

import Foreign
import Foreign.C.Types(CInt,CUChar)

data CSegment = CSegment {
    seg_x1, seg_y1, seg_x2, seg_y2 :: !Float,
    seg_gray_izq, seg_gray_der :: !CUChar,
    seg_r_izq, seg_r_der, seg_g_izq, seg_g_der, seg_b_izq,b_der :: !CUChar,
    seg_num_points :: !CInt,
    seg_cx, seg_cy, seg_angle, seg_length :: !Float,
    seg_desv_perp :: !Float
  }

instance Storable CSegment where
    sizeOf _ = #size TSegment
    alignment _ = #alignment TSegment
    peek ptr = do
        x1 <- (#peek TSegment, x1) ptr
        y1 <- (#peek TSegment, y1) ptr
        x2 <- (#peek TSegment, x2) ptr
        y2 <- (#peek TSegment, y2) ptr
        gray_izq <- (#peek TSegment, gray_izq) ptr
        gray_der <- (#peek TSegment, gray_der) ptr
        r_izq <- (#peek TSegment, r_izq) ptr
        r_der <- (#peek TSegment, r_der) ptr
        g_izq <- (#peek TSegment, g_izq) ptr
        g_der <- (#peek TSegment, g_der) ptr
        b_izq <- (#peek TSegment, b_izq) ptr
        b_der <- (#peek TSegment, b_der) ptr
        np <- (#peek TSegment, num_points) ptr
        cx <- (#peek TSegment, cx) ptr
        cy <- (#peek TSegment, cy) ptr
        ang <- (#peek TSegment, angle) ptr
        len <- (#peek TSegment, length) ptr
        dp <- (#peek TSegment, desv_perp) ptr
        return $ CSegment x1 y1 x2 y2 gray_izq gray_der r_izq r_der g_izq g_der b_izq b_der np cx cy ang len dp
    poke ptr _ = error $ "TSegment poke not implemented"

{-
typedef struct {
    float x1,y1,x2,y2;                                  /* segment extremes */
    unsigned char gray_izq,gray_der;                    /* median gray values */
    unsigned char r_izq,r_der,g_izq,g_der,b_izq,b_der;  /* median RGB values */
    int num_points;                                     /* number of captured high-pass points */
    float cx,cy,angle,length;                           /* center, angle, and length */
    float desv_perp;                                    /* std dev in perpendicular direction */
} TSegment;
-}

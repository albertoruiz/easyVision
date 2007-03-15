{-# OPTIONS -fffi #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Segments
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Interface to the segment extractor by Pedro E. Lopez de Teruel.

-}
-----------------------------------------------------------------------------

module ImagProc.Segments (
    Segment(..),
    segments
)
where

import Foreign
import Foreign.C.Types
import ImagProc.Ipp
import GHC.Float(float2Double)
import Control.Monad (when)

foreign import ccall "Segments/segments.h mycvSegmentsWithParms_8u_C1_C3"
    c_segments :: Ptr () -> Int -> 
                  Int -> Int -> Double ->
                  Ptr (Ptr ()) ->
                  Ptr Int ->
                  Int -> Float ->
                  Int ->
                  CUChar -> CUChar -> Int -> IO ()

foreign import ccall "Segments/segments.h mycvPostProcessSegments"
    c_post_process_segments :: Ptr (Ptr ()) ->
                               Ptr Int ->
                               Float -> Float ->
                               CUChar -> IO ()

data Segment = Segment {
    extreme1 :: Point,
    extreme2 :: Point
}

-- | Segment extractor
segments :: Int   -- ^ user radius (eg., 4)
         -> Float -- ^ edgel width (eg., 1.5)
         -> Int   -- ^ median size (eg., 5)
         -> CUChar -- ^ high threshold (eg., 40)
         -> CUChar -- ^ low threshold (eg., 20)
         -> Bool      -- ^ post process segments
         -> ImageGray -- ^ input image
         -> [Segment] -- ^ result
segments rad we medsz th tl pp (G im) = unsafePerformIO $ do
    pi <- malloc
    pn <- malloc
    let ROI r1 r2 c1 c2 = vroi im `intersection` (shrink (15,15) (fullroi im))
        start = plusPtr (ptr im) (r1* step im + c1)
    c_segments start (step im) c1 r1 (ippRect (c2-c1+1) (r2-r1+1)) pi pn rad we medsz th tl 1
    when pp (c_post_process_segments pi pn 8 30 0)
    n <- peek pn
    ps <- peek pi
    let Size h' w' = isize im
        h = fromIntegral h'
        w = fromIntegral w'
        r = (h+1)/(w+1)
    segs <- mapM (segment ps h w r) [0 .. n-1]
    touchForeignPtr (fptr im)
    free ps
    return segs

segment p h w r k = do
    let jump = (4+5) * sizeOf (undefined ::Float) + 8 + sizeOf (undefined :: Int)
    --let q = advancePtr (castPtr p) (k*jump) :: Ptr Char
    let q = plusPtr p (k*jump)
    s@[x1,y1,x2,y2] <- peekArray 4 (castPtr q :: Ptr Float)
    return Segment { extreme1 = Point (1-2*float2Double x1/w) (r-2*float2Double y1*r/h)
                   , extreme2 = Point (1-2*float2Double x2/w) (r-2*float2Double y2*r/h)
                   }

{-
typedef struct {
    float x1,y1,x2,y2; /* Extremos del segmento. */
    unsigned char gray_izq,gray_der; /* Valores de gris medianos. */
    unsigned char r_izq,r_der,g_izq,g_der,b_izq,b_der; /* RGB medianos. */
    int num_points; /* Nmero de puntos pasa-alta capturados. */
    float cx,cy,angle,length; /* Centro, �gulo y longitud del segmento.*/
    float desv_perp; /* Desviaci� t�ica en direcci� perpendicular. */
} TSegment;
-}
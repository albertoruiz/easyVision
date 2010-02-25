{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.C.Segments
Copyright   :  (c) Alberto Ruiz 2007-2010
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Interface to the segment extractor by Pedro E. Lopez de Teruel.

-}
-----------------------------------------------------------------------------

module ImagProc.C.Segments (   
    segments,
)
where

import Prelude hiding (pi)
import Foreign
import ImagProc.Ipp hiding (r1,c1,r2,c2)
import GHC.Float(float2Double)
import Control.Monad (when)
import ImagProc.C.Segments.Structs

foreign import ccall "Segments/mycvSegments.h mycvSegmentsWithParms_8u_C1_C3"
    c_segments :: Ptr CUChar -> Int ->
                  Int -> Int -> Ptr IppiSize ->
                  Ptr (Ptr CSegment) ->
                  Ptr CInt ->
                  Int -> Float ->
                  Int ->
                  CUChar -> CUChar -> Int -> IO ()

foreign import ccall "Segments/mycvSegments.h mycvPostProcessSegments"
    c_post_process_segments :: Ptr (Ptr CSegment) ->
                               Ptr CInt ->
                               Float -> Float ->
                               CUChar -> IO ()

-- | Extracts a list of segments from an image.
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
    let r@(ROI r1 _r2 c1 _c2) = shrink (1,1) $ vroi im `intersection` (shrink (15,15) (fullroi im))
        start = plusPtr (ptr im) (r1* step im + c1)
    pr <- malloc 
    poke pr (roiSZ r)
    (c_segments start (step im) c1 r1 pr) pi pn rad we medsz th tl 1
    when pp (c_post_process_segments pi pn 8 30 0)
    n <- peek pn
    ps <- peek pi
    let Size h' w' = isize im
        h = fromIntegral h'
        w = fromIntegral w'
        rat = (h+1)/(w+1)
    csegs <- peekArray (fromIntegral n-1) ps
    touchForeignPtr (fptr im)
    free ps
    free pi
    free pn
    free pr
    return $ map (segment h w rat) csegs

segment :: Double -> Double -> Double -> CSegment -> Segment
segment h w r (CSegment {seg_x1=x1, seg_y1=y1, seg_x2=x2, seg_y2=y2}) =
    Segment { extreme1 = Point (1-2*float2Double x1/w) (r-2*float2Double y1*r/h)
            , extreme2 = Point (1-2*float2Double x2/w) (r-2*float2Double y2*r/h)
            }


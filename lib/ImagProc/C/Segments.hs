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

module ImagProc.C.Segments (
   
    -- * Segment extraction
    segments,
    -- * Extraction of polylines from segments
    segmentsToPolylines
)
where

import Foreign
import Foreign.C.Types
import ImagProc.Ipp
import GHC.Float(float2Double)
import Control.Monad (when)
import Data.Array hiding ((//))
import Data.List(sortBy,(\\))
import Data.Graph
import Data.Tree
import Debug.Trace
import Vision
import Numeric.LinearAlgebra
import Features.Polyline
import ImagProc.C.Segments.Structs

vector = fromList :: [Double] -> Vector Double

debug x = trace (show x) x


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
    let r@(ROI r1 r2 c1 c2) = shrink (1,1) $ vroi im `intersection` (shrink (15,15) (fullroi im))
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
        r = (h+1)/(w+1)
    csegs <- peekArray (fromIntegral n-1) ps
    touchForeignPtr (fptr im)
    free ps
    free pi
    free pn
    free pr
    return $ map (segment h w r) csegs

segment h w r (CSegment {seg_x1=x1, seg_y1=y1, seg_x2=x2, seg_y2=y2}) =
    Segment { extreme1 = Point (1-2*float2Double x1/w) (r-2*float2Double y1*r/h)
            , extreme2 = Point (1-2*float2Double x2/w) (r-2*float2Double y2*r/h)
            }

------------------------------------------------------------------------------


distances segs = do
    let n = length segs
    i <- [1 .. n-1]
    j <- [i+1 .. n]
    let Segment {extreme1 = a1, extreme2 = a2} = segs!!(i-1)
    let Segment {extreme1 = b1, extreme2 = b2} = segs!!(j-1)
    ds <- [(distPoints a1 b1,(i,j)),
           (distPoints a1 b2,(i,-j)),
           (distPoints a2 b1,(-i,j)),
           (distPoints a2 b2,(-i,-j))]
    return ds


createGraph l dsegs = buildG (-l,l) (edgelist ++ map (\(a,b)->(b,a)) edgelist)
    where edgelist = map snd dsegs ++ [(i,-i)|i<- [1..l]]

tk segs a | a > 0     = extreme1 (segs!!(a-1))
          | otherwise = extreme2 (segs!!(-a-1))

extend g (f:ps) = case (g!f) \\ ps of
    []   -> (f:ps)
    a:as -> extend g (a:f:ps)

-- | Given a distance threshold, it tries to link a list of segments to create a list of polylines.
segmentsToPolylines :: Double -> [Segment] -> [Polyline]
segmentsToPolylines r segs = (map (improvePoints.putType.map (tk segs).{-debug.-}okInit.cleanOdd) (filter ((1<).length) pol))
    where dists = filter ((<r).fst) (distances segs)
          g = createGraph (length segs) dists
          pol = map (extend g.reverse.extend g.return.rootLabel) (components g)
          putType l | isClosed r l = Closed l
                    | otherwise    = Open   l

okInit l@(a:b:rest) | a == -b          = l
                    | a == - last rest = b:rest ++ [a]
                    | otherwise        = b:rest

isClosed r p = distPoints (head p) (last p) < r

-- | Replaces the extremes of consecutive segments by the intersection of such segments
improvePoints :: Polyline -> Polyline
improvePoints (Open l)              = Open   (betterPoints $ auxLines' l)
improvePoints (Closed l@(a:b:rest)) = clockwise $ Closed (betterPoints $ auxLines' (l++[a,b]))

auxLines' [] = []
auxLines' (Point x1 y1: Point x2 y2 :rest) = cross (vector [x1,y1,1]) (vector [x2,y2,1]) : auxLines' rest

betterPoints [_] = []
betterPoints (a:b:rest) = toPoint (inHomog (cross a b)) : betterPoints (b:rest)
    where toPoint v = let [x,y] = toList v in Point x y

cleanOdd [a,b] | a == -b = [a,b]
               | otherwise = [a]
cleanOdd (a:b:c:rest) | b==(-a) || b==(-c) = a: cleanOdd (b:c:rest)
                      | otherwise = cleanOdd (a:c:rest)

clockwise p@(Closed l) | orientation p < 0 = p  -- in the camera frame clockwise orientation is negative
                       | otherwise         = Closed (reverse l)

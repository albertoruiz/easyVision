-- Written by Adrian Quark, 2007
-- vim: set et ts=2 sw=2:
{-# LANGUAGE ForeignFunctionInterface #-}
module ImagProc.C.Burns where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import ImagProc.Ipp

#include "Burns/burns.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data Line = Line Double Double Double Double
  deriving (Show)

instance Storable Line where
  sizeOf _ = #size Line
  alignment _ = #alignment Line
  peek ptr = do
    x1 <- (#peek Line, x1) ptr
    y1 <- (#peek Line, y1) ptr
    x2 <- (#peek Line, x2) ptr
    y2 <- (#peek Line, y2) ptr
    return (Line x1 y1 x2 y2)
  poke ptr (Line x1 y1 x2 y2) = do
    (#poke Line, x1) ptr x1
    (#poke Line, y1) ptr y1
    (#poke Line, x2) ptr x2
    (#poke Line, y2) ptr y2

foreign import ccall unsafe "static Burns/burns.h burns_line_extraction"
  c_burns_line_extraction :: Ptr () -> Int -> Int -> Int -> Int -> Int -> Double -> Ptr (Ptr Line) -> Ptr CInt -> IO ()

foreign import ccall unsafe "static Burns/burns.h free_lines"
  c_free_lines :: Ptr Line -> IO ()

foreign import ccall unsafe "static Burns/burns.h print_lines"
  c_print_lines :: Ptr Line -> Int -> IO ()

foreign import ccall unsafe "stdlib.h &free"
  p_c_free :: FunPtr (Ptr () -> IO ())

instance Show Segment where
  show (Segment p1 p2) = "{Segment extreme1=" ++ show p1 ++ ", extreme2=" ++ show p2 ++ "}"

segment :: Int -> Int -> Size -> Line -> Segment
segment xoff' yoff' (Size h' w') (Line x1 y1 x2 y2) =
  let h = fromIntegral h'
      w = fromIntegral w'
      xoff = fromIntegral xoff'
      yoff = fromIntegral yoff'
      r = h/w
      toPoint x y = Point (1-2*x/w) (r-2*y*r/h)
  in Segment (toPoint (xoff + x1) (yoff + y1))
     (toPoint (xoff + x2) (yoff + y2))

burns_line_extraction ::
  ImageGray -- original image
  -> Int -- number of buckets
  -> Int -- minimum gradient
  -> Double -- minimum line length
  -> [Segment] -- list of segments
burns_line_extraction (G image) num_buckets min_gradient min_length = unsafePerformIO $ do
  alloca $ \lines_ptr -> alloca $ \num_lines_ptr -> do
    let ROI r1 r2 c1 c2 = (vroi image)
        image_ptr = plusPtr (ptr image) (r1 * (step image) + c1)
    c_burns_line_extraction image_ptr (step image) (c2-c1+1) (r2-r1+1)
      num_buckets min_gradient min_length lines_ptr num_lines_ptr
    lines <- peek lines_ptr
    num_lines <- peek num_lines_ptr
    lines_list <- mapM (peekElemOff lines) [0 .. fromIntegral num_lines-1]
    --mapM_ (putStrLn . show) lines_list
    c_free_lines lines
    let segments = map (segment c1 r1 (isize image)) lines_list
    --mapM_ (putStrLn . show) segments
    return segments


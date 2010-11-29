-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Base
Copyright   :  (c) Alberto Ruiz 2006-8
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

General types.

-}
-----------------------------------------------------------------------------

module ImagProc.Base
( Size(..),
  ROI (..),
  Point(..), distPoints,
  Pixel(..),
  pixelsToPoints, pointsToPixels, pixelToPointTrans,
  Segment(..),
  segmentLength,
  InterestPoint(..)
) where

import qualified Numeric.LinearAlgebra as LA
import Vision

data Size  = Size  {height :: !Int, width :: !Int} deriving (Show, Eq)



data ROI = ROI { r1 :: Int  -- ^ upper row
               , r2 :: Int  -- ^ lower row
               , c1 :: Int  -- ^ leftmost column
               , c2 :: Int  -- ^ rightmost column
               } deriving (Show,Read,Eq)

-- -- | Normalized image coordinates, with x from +1 to -1 (for a right handed 3D reference system with z pointing forward)
-- data Point = Point { px    :: !Double, py :: !Double} deriving (Eq, Show)

-- | Raw image coordinates
data Pixel = Pixel { row   :: !Int,    col :: !Int } deriving (Eq, Show)

-- | Auxiliary homogeneous transformation from 'Pixel's to 'Point's
pixelToPointTrans :: Size -> LA.Matrix Double
pixelToPointTrans Size {width = w', height = h'} = nor where
    w = fromIntegral w'
    h = fromIntegral h'
    r = h/w
    nor = LA.fromLists
        [[-2/w,      0, 1]
        ,[   0, -2*r/h, r]
        ,[   0,      0, 1]]

pixelToList Pixel {row = r, col = c} = [fromIntegral c, fromIntegral r]
pointToList p = [px p, py p]
listToPoint [x,y] = Point {px = x, py= y}
listToPixel [c,r] = Pixel {row = round r, col = round c}

-- | Trasformation from pixels to normalized points.
pixelsToPoints :: Size -> [Pixel]->[Point]
pixelsToPoints sz [] = []
pixelsToPoints sz ps = fix ps where
    nor = pixelToPointTrans sz
    fix = map listToPoint. ht nor . map pixelToList

-- | Trasformation from pixels to normalized points.
pointsToPixels :: Size -> [Point]->[Pixel]
pointsToPixels sz [] = []
pointsToPixels sz ps = fix ps where
    nor = LA.inv (pixelToPointTrans sz)
    fix = map listToPixel. ht nor . map pointToList


data Segment = Segment {
    extreme1 :: !Point,
    extreme2 :: !Point
} deriving (Show)

-- | The length of a segment.
segmentLength :: Segment -> Double
segmentLength (Segment {extreme1 = e1, extreme2 = e2}) = distPoints e1 e2

-- | Euclidean distance between two points
distPoints :: Point -> Point -> Double
distPoints (Point a b) (Point x y) = sqrt $ (a-x)^2+(b-y)^2

-----------------------------------------------------------------------------

data InterestPoint = IP {
      ipPosition    :: Point
    , ipScale       :: Double
    , ipOrientation :: Double
    , ipDescriptor  :: LA.Vector Double
    } deriving (Eq, Show)

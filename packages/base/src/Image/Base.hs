-----------------------------------------------------------------------------
{- |
Module      :  Image.Base
Copyright   :  (c) Alberto Ruiz 2006-12
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

General types.

-}
-----------------------------------------------------------------------------

module Image.Base
( Size(..), limitSize, shSize,
  ROI (..),
  Point(..), distPoints, bounding,
  Pixel(..),
  pixelsToPoints, pointsToPixels, pixelToPointTrans,
  Segment(..),
  segmentLength, cosAngleSegments,
  InterestPoint(..),
  Polyline(..),
  HLine(..)
) where

import qualified Numeric.LinearAlgebra as LA
import Util.Geometry(Point(..),HLine(..))
import Util.Homogeneous(ht)

data Size  = Size  {height :: !Int, width :: !Int} deriving (Show, Eq)

shSize :: Size -> String
shSize (Size h w) = show w ++"x"++ show h

limitSize :: Int -> Size -> Size
limitSize mx (Size h w)
    | s <= mx = (Size h w)
    | otherwise = (Size h' w')
  where
    s = max h w
    r = fromIntegral w /  fromIntegral h
    (h',w') | w > h     = (round (fromIntegral mx/r), mx)
            | otherwise = (mx, round (fromIntegral mx*r))




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

--------------------------------------------------------------------------------

data Polyline = Closed { polyPts :: [Point] }
              | Open   { polyPts :: [Point] } deriving (Show,Read)

--------------------------------------------------------------------------------

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

bounding :: Polyline -> Polyline
bounding p = Closed [Point x2 y2, Point x1 y2, Point x1 y1, Point x2 y1] 
  where
    x1 = minimum xs
    x2 = maximum xs
    y1 = minimum ys
    y2 = maximum ys
    xs = map px (polyPts p)
    ys = map py (polyPts p)

--------------------------------------------------------------------------------


cosAngleSegments :: Segment -> Segment -> Double
cosAngleSegments (Segment p q) (Segment p' q') = ca
  where
     Point x0 y0 = p
     Point x1 y1 = q
     Point x0' y0' = p'
     Point x1' y1' = q'
     ux = x1-x0
     uy = y1-y0
     vx = x1'-x0'
     vy = y1'-y0'
     u2 = ux*ux+uy*uy
     v2 = vx*vx+vy*vy
     uv = ux*vx+uy*vy
     ca = uv/(sqrt (abs u2)*sqrt (abs v2))
-----------------------------------------------------------------------------

data InterestPoint = IP {
      ipPosition    :: Point
    , ipScale       :: Double
    , ipOrientation :: Double
    , ipDescriptor  :: LA.Vector Double
    } deriving (Eq, Show)

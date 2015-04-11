-----------------------------------------------------------------------------
{- |
Module      :  Image.Types
Copyright   :  (c) Alberto Ruiz 2006-13
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module Image.Types
( Size(..), limitSize, shSize,
  ROI (..),
  Pixel(..),
  pixelsToPoints, pointsToPixels, pixelToPointTrans,
  InterestPoint(..),
) where

import Util.Geometry(Point(..))
import Numeric.LinearAlgebra.HMatrix
import Util.Homogeneous(ht)
import Data.Binary

data Size  = Size  {height :: !Int, width :: !Int}
  deriving (Show, Eq)

instance Binary Size
  where
    put (Size h w) = put h >> put w
    get = Size <$> get <*> get

shSize :: Size -> String
shSize (Size h w) = show w ++"x"++ show h

limitSize :: Int -> Size -> Size
limitSize mx (Size h w)
    | s <= mx = (Size h w)
    | otherwise = (Size h' w')
  where
    s = max h w
    r = fromIntegral w /  fromIntegral h :: Double
    (h',w') | w > h     = (round (fromIntegral mx/r), mx)
            | otherwise = (mx, round (fromIntegral mx*r))


-- | ROI (upper row) (lower row) (leftmost column) (rightmost column)
data ROI = ROI !Int !Int !Int !Int
  deriving (Show,Read,Eq)

instance Binary ROI
  where
    put (ROI r1 r2 c1 c2) = put r1 >> put r2 >> put c1 >> put c2
    get = ROI <$> get <*> get <*> get <*> get


-- | Raw image coordinates, Pixel row column
data Pixel = Pixel !Int !Int deriving (Eq, Show)


-- | Auxiliary homogeneous transformation from 'Pixel's to 'Point's
pixelToPointTrans :: Size -> Matrix Double
pixelToPointTrans Size {width = w', height = h'} = nor where
    w = fromIntegral w'
    h = fromIntegral h'
    r = h/w
    nor = (3><3)
        [-2/w,      0, 1
        ,   0, -2*r/h, r
        ,   0,      0, 1]

pixelToList :: Pixel -> [Double]
pixelToList (Pixel r c) = [fromIntegral c, fromIntegral r]

pointToList :: Point -> [Double]
pointToList (Point x y) = [x, y]

listToPoint :: [Double] -> Point
listToPoint [x,y] = Point x y
listToPoint _ = error "listToPoint"

listToPixel :: [Double] -> Pixel
listToPixel [c,r] = Pixel (round r) (round c)
listToPixel _ = error "listToPixel"

-- | Trasformation from pixels to normalized points, with x from +1 to -1 (for a right handed 3D reference system with z pointing forward).
pixelsToPoints :: Size -> [Pixel]->[Point]
pixelsToPoints _ [] = []
pixelsToPoints sz ps = fix ps where
    nor = pixelToPointTrans sz
    fix = map listToPoint. ht nor . map pixelToList

-- | Trasformation from pixels to normalized points.
pointsToPixels :: Size -> [Point]->[Pixel]
pointsToPixels _ [] = []
pointsToPixels sz ps = fix ps where
    nor = inv (pixelToPointTrans sz)
    fix = map listToPixel. ht nor . map pointToList

-----------------------------------------------------------------------------

data InterestPoint = IP {
      ipPosition    :: Point
    , ipScale       :: Double
    , ipOrientation :: Double
    , ipDescriptor  :: Vector Double
    } deriving (Eq, Show)


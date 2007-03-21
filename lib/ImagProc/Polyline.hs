-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Polyline
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Some operations with polylines.

-}
-----------------------------------------------------------------------------

module ImagProc.Polyline (
    Polyline(..),
    perimeter,
    orientation,
)
where

import ImagProc.Images

data Polyline = Closed [Point]
              | Open   [Point]

distPoints (Point a b) (Point x y) = (a-x)^2+(b-y)^2

-- | (for an open polyline is the length)
perimeter :: Polyline -> Double
perimeter (Open l) = perimeter' l
perimeter (Closed l) = perimeter' (last l:l)

perimeter' [_] = 0
perimeter' (a:b:rest) = distPoints a b + perimeter' (b:rest)

-- | Oriented area of a closed polyline. The clockwise sense is positive in the x-y world frame (\"floor\",z=0) and negative in the camera frame.
--
-- area = abs.orientation.
orientation :: Polyline -> Double
orientation (Open _) = error "undefined orientation of open polyline"
orientation (Closed l) = -0.5 * orientation' (last l:l)

orientation' [_] = 0
orientation' (Point x1 y1:r@(Point x2 y2:_)) = x1*y2-x2*y1 + orientation' r

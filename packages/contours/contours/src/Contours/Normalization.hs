-----------------------------------------------------------------------------
{- |
Module      :  Contours.Normalization
Copyright   :  (c) Alberto Ruiz 2007-11
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Shape normalization and whitening.

-}
-----------------------------------------------------------------------------

module Contours.Normalization (
    centerShape,
    normalShape,
    boxShape,
    whitenContour, whitener, whitener', equalizeContour,
    momentsContour, momentsBoundary,
    eig2x2Dir
)
where

import Contours.Base
import Data.List(foldl')
import Numeric.LinearAlgebra
import Util.Homogeneous
import Util.Rotation(rot3)
import Util.Geometry(segmentLength, bounding)


auxContour (s,sx,sy,sx2,sy2,sxy) seg@(Segment (Point x1 y1) (Point x2 y2))
    = (s+l,
       sx+l*(x1+x2)/2,
       sy+l*(y1+y2)/2,
       sx2+l*(x1*x1 + x2*x2 + x1*x2)/3,
       sy2+l*(y1*y1 + y2*y2 + y1*y2)/3,
       sxy+l*(2*x1*y1 + x2*y1 + x1*y2 + 2*x2*y2)/6)
  where l = segmentLength seg

auxSolid (s,sx,sy,sx2,sy2,sxy) seg@(Segment (Point x1 y1) (Point x2 y2))
    = (s   + (x1*y2-x2*y1)/2,
       sx  + ( 2*x1*x2*(y2-y1)-x2^2*(2*y1+y2)+x1^2*(2*y2+y1))/12,
       sy  + (-2*y1*y2*(x2-x1)+y2^2*(2*x1+x2)-y1^2*(2*x2+x1))/12,
       sx2 + ( (x1^2*x2+x1*x2^2)*(y2-y1) + (x1^3-x2^3)*(y1+y2))/12,
       sy2 + (-(y1^2*y2+y1*y2^2)*(x2-x1) - (y1^3-y2^3)*(x1+x2))/12,
       sxy + ((x1*y2-x2*y1)*(x1*(2*y1+y2)+x2*(y1+2*y2)))/24)

moments2Gen method l = (mx,my,cxx,cyy,cxy)
    where (s,sx,sy,sx2,sy2,sxy) = (foldl' method (0,0,0,0,0,0). asSegments . Closed) l
          mx = sx/s
          my = sy/s
          cxx = sx2/s - mx*mx
          cyy = sy2/s - my*my
          cxy = sxy/s - mx*my

-- | Mean and covariance matrix of a continuous piecewise-linear contour.
momentsContour :: [Point] -- ^ closed polyline
                 -> (Double,Double,Double,Double,Double) -- ^ (mx,my,cxx,cyy,cxy)
momentsContour = moments2Gen auxSolid

-- | Mean and covariance matrix of the boundary of a continuous piecewise-linear contour.
momentsBoundary :: [Point] -- ^ closed polyline
                 -> (Double,Double,Double,Double,Double) -- ^ (mx,my,cxx,cyy,cxy)
momentsBoundary = moments2Gen auxContour

-- | Structure of a 2x2 covariance matrix
eig2x2Dir :: (Double,Double,Double) -- ^ (cxx,cyy,cxy)
          -> (Double,Double,Double) -- ^ (v1,v2,angle), the eigenvalues of cov (v1>v2), and angle of dominant eigenvector
eig2x2Dir (cxx,cyy,cxy) = (l1,l2,a')
    where ra = sqrt(abs $ cxx*cxx + 4*cxy*cxy -2*cxx*cyy + cyy*cyy)
          l1 = 0.5*(cxx+cyy+ra) `max` 0
          l2 = 0.5*(cxx+cyy-ra) `max` 0
          a = atan2 (2*cxy) ((cxx-cyy+ra))
          a' | abs cxy < eps && cyy > cxx = pi/2
             | otherwise = a

-- | Equalizes the eigenvalues of the covariance matrix of a continuous piecewise-linear contour. It preserves the general scale, position and orientation.
equalizeContour :: Polyline -> Polyline
equalizeContour c@(Closed ps) = transPol t c where
    (mx,my,cxx,cyy,cxy) = momentsContour ps
    (l1,l2,a) = eig2x2Dir (cxx,cyy,cxy)
    t = desp (mx,my) <> rot3 (-a) <> diag (fromList [sqrt (l2/l1),1,1]) <> rot3 (a) <> desp (-mx,-my)

-- | Finds a transformation that equalizes the eigenvalues of the covariance matrix of a continuous piecewise-linear contour. It is affine invariant modulo rotation.
whitener :: Polyline -> Matrix Double
whitener (Closed ps) = t where
    (mx,my,cxx,cyy,cxy) = momentsContour ps
    (l1,l2,a) = eig2x2Dir (cxx,cyy,cxy)
    --t = diag (fromList [1/sqrt l1,1/sqrt l2,1]) <> rot3 (a) <> desp (-mx,-my)
    t = whitener' (momentsContour ps)

-- | Based on closed-form Cholesky Factorization
whitener' :: (Double,Double,Double,Double,Double) -> Matrix Double
whitener' (mx,my,cxx,cyy,cxy) = (3><3) [ a, b, -a*mx-b*my
                                       , 0, c, -c*my
                                       , 0, 0,   1       ]
  where
    delta = sqrt( cxx / (cxx*cyy-cxy*cxy) )
    a = 1/sqrt cxx
    b = -cxy/cxx*delta
    c = delta


whitenContour t = transPol w t
  where
    w = whitener t


-- | centered
centerShape :: Polyline -> Polyline
centerShape c = transPol h c
 where
   (x,y,_,_,_) = momentsContour (polyPts c)
   h = desp (-x,-y)

-- | centered and unit max std
normalShape :: Polyline -> Polyline
normalShape c = transPol h c
 where
   (x,y,sx,sy,_) = momentsContour (polyPts c)
   h = scaling (1/ sqrt( max sx sy)) <> desp (-x,-y)

-- | centered and the middle of bounding box of height 2
boxShape :: Polyline -> Polyline
boxShape c = transPol h c
  where
    Closed [Point x2 y2, _, Point x1 y1, _] = bounding c
    h = scaling (2/(y2-y1)) <> desp (-(x1+x2)/2,-(y1+y2)/2)


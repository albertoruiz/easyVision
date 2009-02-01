-----------------------------------------------------------------------------
{- |
Module      :  Features.Polyline
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Some operations with polylines.

-}
-----------------------------------------------------------------------------

module Features.Polyline (
-- * Operations
    Polyline(..),
    perimeter,
    orientation,
    whitenContour, whitener,
    fourierPL,
    norm2Cont,
-- * Reduction
    douglasPeucker, douglasPeuckerClosed,
    selectPolygons,
-- * Extraction
    rawContour,
    contours,
    contourAt,
-- * Auxiliary tools
    momentsContour, momentsBoundary,
    eig2x2Dir, asSegments, longestSegments
)
where

import ImagProc.Images
import ImagProc.ImageProcessing(cloneClear,maxIndx8u,floodFill8u, floodFill8uGrad, binarize8u, median, notI)
import ImagProc.Ipp.Core
import Foreign.C.Types(CUChar)
import Foreign
import Debug.Trace
import Data.List(maximumBy, zipWith4, sort)
import Numeric.LinearAlgebra
import Vision.Geometry

debug x = trace (show x) x

data Polyline = Closed [Point]
              | Open   [Point]

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

--------------------------------------------------------------

data Dir = ToRight | ToLeft | ToDown | ToUp deriving Eq
nextPos :: ImageGray -> CUChar -> (Pixel,Dir) -> (Pixel,Dir)

nextPos im v (Pixel r c, ToRight) = case (a,b) of
    (False,False) -> (Pixel (r+1) c, ToDown)
    (False,True)  -> (Pixel r (c+1), ToRight)
    _             -> (Pixel (r-1) c, ToUp)
  where
    a = val8u im (Pixel (r-1) c) == v
    b = val8u im (Pixel r c) == v

nextPos im v (Pixel r c, ToDown) = case (a,b) of
    (False,False) -> (Pixel r (c-1), ToLeft)
    (False,True)  -> (Pixel (r+1) c, ToDown)
    _             -> (Pixel r (c+1), ToRight)
  where
    a = val8u im (Pixel r c) == v
    b = val8u im (Pixel r (c-1)) == v

nextPos im v (Pixel r c, ToLeft) = case (a,b) of
    (False,False) -> (Pixel (r-1) c, ToUp)
    (False,True)  -> (Pixel r (c-1), ToLeft)
    _             -> (Pixel (r+1) c, ToDown)
  where
    a = val8u im (Pixel r (c-1)) == v
    b = val8u im (Pixel (r-1) (c-1)) == v

nextPos im v (Pixel r c, ToUp) = case (a,b) of
    (False,False) -> (Pixel r (c+1), ToRight)
    (False,True)  -> (Pixel (r-1) c, ToUp)
    _             -> (Pixel r (c-1), ToLeft)
  where
    a = val8u im (Pixel (r-1) (c-1)) == v
    b = val8u im (Pixel (r-1) c) == v


-- | extracts a contour with given value from an image.
--   Don't use it if the region touches the limit of the image ROI.
rawContour :: ImageGray -- ^ source image
           -> Pixel     -- ^ starting point of the contour (a top-left corner)
           -> CUChar    -- ^ pixel value of the region (typically generated by some kind of floodFill or thresholding)
           -> [Pixel]   -- ^ contour of the region
rawContour im start v = clean $ iterate (nextPos im v) (start, ToRight)
    where clean ((a,_):rest) = a : clean' a rest
          clean' p ((v1,s1):rest@((v2,s2):_))
            | p  == v1  = []
            | s1 == s2  = clean' p rest
            | otherwise = v1: clean' p rest



-- | extracts a list of contours in the image
contours :: Int       -- ^ maximum number of contours
         -> Int       -- ^ minimum area (in pixels) of the admissible contours
         -> CUChar    -- ^ binarization threshold
         -> Bool      -- ^ binarization mode (True/False ->detect white/black regions)
         -> ImageGray -- ^ image source
         -> [([Pixel],Int,ROI)]  -- ^ list of contours, with area and ROI
contours n d th mode im = unsafePerformIO $ do
    aux <- cloneClear $ (if mode then id else notI) (binarize8u th im)
    r <- auxCont n d aux
    return r



auxCont n d aux = do
    let (v,p) = maxIndx8u aux
    if n==0 || (v<255)
        then return []
        else do
            (r@(ROI r1 r2 c1 c2),a,_) <- floodFill8u aux p 128
            let ROI lr1 lr2 lc1 lc2 = theROI aux
            if a < d || r1 == lr1 || c1 == lc1 || r2 == lr2 || c2 == lc2
                    then auxCont n d aux
                    else do
                    let c = rawContour aux p 128
                    rest <- auxCont (n-1) d aux
                    return ((c,a,r):rest)


contourAt :: Int -> ImageGray -> Pixel -> Maybe [Pixel]
contourAt dif img start = unsafePerformIO $ do
    aux <- cloneClear (median Mask5x5 img)
    let ROI lr1 lr2 lc1 lc2 = theROI aux
        d = fromIntegral dif
    (r@(ROI r1 r2 c1 c2),a,_) <- floodFill8uGrad aux start d d 0
    let st = findStart aux start
        touches = r1 == lr1 || c1 == lc1 || r2 == lr2 || c2 == lc2
        pol = if not touches
                then Just (rawContour aux st 0)
                else Nothing
    return pol

findStart im = fixp (findLimit im left . findLimit im top)

findLimit im dir pix
    | val8u im neig == 0 = findLimit im dir neig
    | otherwise          = pix
  where neig = dir pix

top  (Pixel r c) = Pixel (r-1) c
left (Pixel r c) = Pixel r (c-1)

fixp f p = if s == p then p else fixp f s
    where s = f p

----------------------------------------------------------------------

-- | Removes nodes in closed polyline such that the orthogonal distance 
--   from the remaining line is less than a given epsilon
douglasPeuckerClosed :: Double -> [Pixel] -> [Pixel]
douglasPeuckerClosed eps (a:b:ls) = b : case criticalPoint (eps^2) b a ls of
    Nothing -> [b]
    Just c  -> left ++ right where
        (l,_:r) = break (==c) ls
        left = douglasPeucker' (eps^2) b c l
        right = douglasPeucker' (eps^2) c a r

-- | Removes nodes in an open polyline such that the orthogonal distance 
--   from the remaining line is less than a given epsilon
douglasPeucker :: Double -> [Pixel] -> [Pixel]
douglasPeucker eps list = a: douglasPeucker' (eps^2) a b list
    where a = head list
          b = last list

douglasPeucker' eps2 a b ls = case criticalPoint eps2 a b ls of
    Nothing -> [b]
    Just c  -> left ++ right where
        (l,_:r) = break (==c) ls
        left = douglasPeucker' eps2 a c l
        right = douglasPeucker' eps2 c b r

perpDistAux :: Int -> Int -> Double -> Int -> Int -> Int -> Int -> Double
perpDistAux lx ly l2 x1 y1 x3 y3 = d2 where
    d2 = p2 - a'*a'/l2
    p2   = fromIntegral $ px*px + py*py
    px   = x3-x1
    py   = y3-y1
    a'   = fromIntegral $ lx*px+ly*py

perpDist (Pixel x1 y1) (Pixel x2 y2) = (f,l2) where
    lx = x2-x1
    ly = y2-y1
    l2 = fromIntegral $ lx*lx+ly*ly
    f (Pixel x3 y3) = perpDistAux lx ly l2 x1 y1 x3 y3

on f g = \x y -> f (g x) (g y)

criticalPoint eps p1 p2 [] = Nothing

criticalPoint eps2 p1 p2 p3s = r where
    (f,l2) = perpDist p1 p2
    p3 = maximumBy (compare `on` f) p3s
    r = if f p3 > eps2
        then Just p3
        else Nothing

---------------------------------------------------------------------------

asSegments :: Polyline -> [Segment]
asSegments (Open ps') = zipWith Segment ps' (tail ps')
asSegments (Closed ps) = asSegments $ Open $ ps++[head ps]

--asSegmentsClosed ps = zipWith Segment ps' (tail ps') where ps' = ps++[head ps]

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
    where (s,sx,sy,sx2,sy2,sxy) = (foldl method (0,0,0,0,0,0). asSegments . Closed) l
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
eig2x2Dir (cxx,cyy,cxy) = (l1,l2,a)
    where ra = sqrt(cxx*cxx + 4*cxy*cxy -2*cxx*cyy + cyy*cyy)
          l1 = 0.5*(cxx+cyy+ra)
          l2 = 0.5*(cxx+cyy-ra)
          a = atan2 (2*cxy) ((cxx-cyy+ra))

-- | Equalizes the eigenvalues of the covariance matrix of a continuous piecewise-linear contour. It preserves the general scale, position and orientation.
whitenContour :: Polyline -> Polyline
whitenContour (Closed ps) = Closed wps where
    (mx,my,cxx,cyy,cxy) = momentsContour ps
    (l1,l2,a) = eig2x2Dir (cxx,cyy,cxy)
    t = desp (mx,my) <> rot3 (-a) <> diag (fromList [sqrt (l2/l1),1,1]) <> rot3 (a) <> desp (-mx,-my)
    p2l (Point x y) = [x,y]
    l2p [x,y] = Point x y
    wps = map l2p $ ht t (map p2l ps)


-- | Finds a transformation that equalizes the eigenvalues of the covariance matrix of a continuous piecewise-linear contour. It is affine invariant modulo rotation.
whitener :: Polyline -> Matrix Double
whitener (Closed ps) = t where
    (mx,my,cxx,cyy,cxy) = momentsContour ps
    (l1,l2,a) = eig2x2Dir (cxx,cyy,cxy)
    t = rot3 (-a) <> diag (fromList [1/sqrt l1,1/sqrt l2,1]) <> rot3 (a) <> desp (-mx,-my)

----------------------------------------------------------

-- | Exact Fourier series of a piecewise-linear closed curve
fourierPL :: Polyline -> (Int -> Complex Double)
fourierPL (Closed ps) = g where
    (zs,ts,cs,ds,hs) = prepareFourierPL ps
    g0 = 0.5 * sum (zipWith4 gamma zs ts (tail zs) (tail ts))
        where gamma z1 t1 z2 t2 = (z2+z1)*(t2-t1)
    -- g w = k*r
    --     where k = recip (-2*pi*i*w'^2)
    --           r = sum (zipWith3 f hs cs ds)
    --           w' = fromIntegral w
    --           f h c d = (h ^^ w)*(w'*c+d)
    g 0 = g0
    g w = k* ((vhs**w') <.> (w'*vcs+vds))
        where k = recip (-2*pi*i* (fromIntegral w)^2)
              w' = fromIntegral w
    vhs = fromList hs
    vcs = fromList $ take (length hs) cs
    vds = fromList $ take (length hs)ds

prepareFourierPL c = (zs,ts,cs,ds,hs) where
    zs = map p2c (c++[head c])
        where p2c (Point x y) = x:+y
    ts = map (/last acclen) acclen
        where acclen = scanl (+) 0 (zipWith sl zs (tail zs))
              sl z1 z2 = abs (z2-z1)
    hs = tail $ map exp' ts
        where exp' t = exp (-2*pi*i*t)
    as = cycle $ zipWith4 alpha zs ts (tail zs) (tail ts)
        where alpha z1 t1 z2 t2 = (z2-z1)/(t2-t1)
    bs = cycle $ zipWith3 beta zs as ts
        where beta z alpha t = z-alpha*t
    aAs = zipWith (-) as (tail as)
    bBs = zipWith (-) bs (tail bs)
    ds = map (* recip (2*pi*i)) aAs
    cs = zipWith3 f bBs (tail (cycle ts)) aAs
        where f b t a = b + t*a

--------------------------------------------------------------------------------
-- | The average squared distance to the origin, assuming a parameterization between 0 and 1.
-- | it is the same as sum [magnitude (f k) ^2 | k <- [- n .. n]] where n is sufficiently large
-- | and f = fourierPL contour
norm2Cont :: Polyline -> Double
norm2Cont c@(Closed ps) = 1/3/perimeter c * go (ps++[head ps]) where
    go [_] = 0
    go (a@(Point x1 y1) : b@(Point x2 y2) : rest) =
        distPoints a b *
        (x1*x1 + x2*x2 + x1*x2 + y1*y1 + y2*y2 + y1*y2)
        + go (b:rest)
--------------------------------------------------------------------------------

longestSegments k poly = filter ok ss
    where ss = asSegments poly
          th = last $ take k $ sort $ map (negate.segmentLength) ss
          ok s = segmentLength s >= -th

reducePolygonTo n poly = Closed $ segsToPoints $ longestSegments n poly

segToHomogLine s = cross (fromList [px $ extreme1 $ s, py $ extreme1 $ s, 1])
                         (fromList [px $ extreme2 $ s, py $ extreme2 $ s, 1])

segsToPoints p = stp $ map segToHomogLine $ p ++ [head p]

stp [] = []
stp [_] = []
stp (a:b:rest) = inter a b : stp (b:rest)

inter l1 l2 = Point x y where [x,y] = toList $ inHomog (cross l1 l2)

tryPolygon eps n poly = if abs((a1-a2)/a1) < eps then [r] else []
    where r = reducePolygonTo n poly
          a1 = orientation poly
          a2 = orientation r

selectPolygons eps n = concat . map (tryPolygon eps n)

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Saddle
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

We extract points with high negative det hessian response (Saddle points in a quadratic model). They seem to have good localization (easy to compute with subpixel accuracy) and a reasonably well defined orientation. We also obtain a descriptor based on the image intensity over a circumference.

-}
-----------------------------------------------------------------------------

module ImagProc.Saddle (
 InterestPoint(..),
 getSaddlePoints
)
where

import ImagProc.Ipp.Core
import ImagProc.ImageProcessing
import Debug.Trace(trace)
import Data.List(zipWith5)
import GSL hiding (size)
import GHC.Float(float2Double,double2Float)

data InterestPoint = IP {
                          ipRawPosition :: Pixel
                        , ipPosition    :: Point
                        , ipOrientation :: Double
                        , ipDescriptor  :: Vector Double
                        , ipTime        :: Int
                        } deriving Show


-- | Returns interest points in the image and a very simple local descriptor.
getSaddlePoints ::
              Int       -- ^ degree of smoothing (e.g. 1)
           -> Int       -- ^ radius of the localmin filter (e.g. 7)
           -> Double    -- ^ fraction of the maximum response allowed (e.g. 0.1)
           -> Int       -- ^ maximum number of interest points
           -> Int       -- ^ dimension of the feature vector
           -> Float     -- ^ radius of the feature circle
           -> ImageFloat  -- ^ source image
           -> IO [InterestPoint]  -- ^ result


taylor gx gy gxx gyy gxy = (delta,vp) where
    d = sqrt (gxx*gxx + gyy*gyy + 4*gxy*gxy - 2*gxx*gyy)
    l1 = 0.5*(gxx+gyy+d)
    l2 = 0.5*(gxx+gyy-d)
    dt = l1*l2
    ok = abs gxy / abs (gxx+gyy) > 1E-6
    a = -0.5*(gyy-gxx-d)/gxy
    na = 1/sqrt(a*a+1)
    b = -0.5*(gyy-gxx+d)/gxy
    nb = 1/sqrt(b*b+1)
    (v1x,v1y,v2x,v2y) =
        if ok then (a*na,-na,b*nb,-nb)
              else (0,1,1,0)
    vp = (v1x, v1y)
    delta = [(gyy*gx-gxy*gy)/dt, (gxy*gx-gxx*gy)/dt]


getSaddlePoints smooth rad prop maxn fn fr im = do
    let suaviza = smooth `times` gauss Mask5x5
    sm <- suaviza im
    gs <- secondOrder sm
    h <- hessian gs >>= scale32f (-1.0)
    (mn,mx) <- minmax h
    hotPixels  <- localMax rad h
               >>= thresholdVal32f (mx* double2Float prop) 0.0 IppCmpLess
               >>= getPoints32f maxn

    let ptp = pixelsToPoints (size im)

    let hotPoints = ptp hotPixels

    let (gx,gy,gxx,gyy,gxy) = gs

    let loc = zip hotPixels hotPoints

    let dir :: (Pixel, Point) -> IO (Pixel, Float)
        dir (pix, Point x y) = do
        dx <- val32f gx pix
        dy <- val32f gy pix
        dxx <- val32f gxx pix
        dyy <- val32f gyy pix
        dxy <- val32f gxy pix
        let (sp,(vx,vy)) = taylor dx dy dxx dyy dxy
        let sg = signum (dx*vx - dy*vy)
        return (pix, atan2 (sg*vy) (sg*vx))

    info <- mapM dir loc
    let dirs = map snd info
    let sp = map fst info
    let cs = zipWith (circle fn fr) hotPixels dirs
    feats' <-  mapM (extractList sm) cs
    let feats = map (fromList . map float2Double) feats'

    let r = zipWith5 IP sp (ptp sp) (map float2Double dirs) feats (repeat 0)

    return r

extractList im l = mapM f l where
    f p = val32f im p

circle :: Int -> Float -> Pixel -> Float -> [Pixel]
circle n rad (Pixel r c) ang = cir where
    d = 2*pi/fromIntegral n
    cir = [Pixel (round $ fromIntegral r-rad*sin (ang+k))
                 (round $ fromIntegral c+rad*cos (ang+k))| k <- [0, d .. 2*pi-d]]

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
 getSaddlePoints,
 basicMatches
)
where

import ImagProc.Ipp.Core
import ImagProc.C.Simple(getPoints32f)
import ImagProc.ImageProcessing
import Data.List(zipWith4)
import Numeric.LinearAlgebra hiding ((.*))
import GHC.Float(float2Double,double2Float)
import Foreign(unsafePerformIO)


-- | Returns interest points in the image and a very simple local descriptor.
getSaddlePoints ::
              Int       -- ^ degree of smoothing (e.g. 1)
           -> Int       -- ^ radius of the localmin filter (e.g. 3)
           -> Float    -- ^ fraction of the maximum response allowed (e.g. 0.1)
           -> Int       -- ^ maximum number of interest points
           -> Int       -- ^ dimension of the feature vector
           -> Float     -- ^ radius of the feature circle
           -> ImageFloat  -- ^ source image
           -> [InterestPoint]  -- ^ result
getSaddlePoints smooth rad prop maxn fn fr im = unsafePerformIO $ do
    let suaviza = (!!smooth) . iterate (gauss Mask5x5)
        sm = suaviza im
        gs = secondOrder sm
        h  = (-1.0) .* hessian gs
        (mn,mx) = minmax h
        hotPixels' = thresholdVal32f (mx*prop) 0.0 IppCmpLess (localMax rad h)
        r = max 0 (ceiling fr - ((rad-1) `div`2))
        hotPixels = getPoints32f maxn (modifyROI (shrink (r,r)) hotPixels')


        ptp = pixelsToPoints (size im)

        hotPoints = ptp hotPixels

        (gx,gy,gxx,gyy,gxy) = gs

        loc = zip hotPixels hotPoints

        dir :: (Pixel, Point) -> IO (Pixel, Float)
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
        sp = map fst info
        cs = zipWith (circle fn fr) hotPixels dirs
    feats' <-  mapM (extractList sm) cs
    let feats = map (fromList . map float2Double) feats'
        Size _ w = size im

        r = zipWith4 IP (ptp sp) (repeat (2*float2Double fr / fromIntegral w)) (map float2Double dirs) feats

    return r


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




extractList im l = mapM f l where
    f p = val32f im p  -- try first with val32f' :)

circle :: Int -> Float -> Pixel -> Float -> [Pixel]
circle n rad (Pixel r c) ang = cir where
    d = 2*pi/fromIntegral n
    cir = [Pixel (round $ fromIntegral r-rad*sin (ang+k))
                 (round $ fromIntegral c+rad*cos (ang+k))| k <- [0, d .. 2*pi-d]]

----------------------------------------------------

corresp umb h w simil = do
    let (v, p@(Pixel r c)) = maxIndx simil
    if (-v) > umb then return []
             else do set32f (-1000) ROI {r1=r,r2=r,c1=0,c2=w-1} simil
                     set32f (-1000) ROI {r1=0,r2=h-1,c1=c,c2=c} simil
                     sig <- corresp umb h w simil
                     return (p:sig)

basicMatches :: ([a],[a]) -> (a -> a -> Double) -> Double -> IO ([a],[a],ImageFloat)
basicMatches (pl,ql) dist umb = do
    let n1 = length pl
    let n2 = length ql
    c <- image (Size n1 n2)
    let t = [[double2Float $ - dist p q | q <- ql] | p <- pl]
    setData32f c t
    let fumb = double2Float umb
    corrs <- corresp fumb n1 n2 c
    let pizq = map ((pl!!).row) corrs
    let pder = map ((ql!!).col) corrs
    return (pizq,pder,c)

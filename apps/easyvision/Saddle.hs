-----------------------------------------------------------------------------
{- |
Module      :  Saddle
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

work in progress

-}
-----------------------------------------------------------------------------

module Saddle (
 InterestPoint(..), ip,
 getSaddlePoints
)
where

import Ipp
import Debug.Trace(trace)
import Data.List(zipWith5)


data InterestPoint = IP {
                          ipRawPosition :: [Double] -- hmm
                        , ipPosition    :: [Double] --hmm
                        , ipOrientation :: Double
                        , ipDescriptor  :: [Double]
                        , ipTime        :: Int
                        } deriving Show

ip p = IP { ipDescriptor  = []
          , ipRawPosition = p
          , ipPosition    = [0,0]
          , ipOrientation = 0
          , ipTime        = 0
          }

-- | Returns improved interest points in the image and an experimental local descriptor.
{-
getSaddlePoints ::
              Int       -- ^ degree of smoothing (e.g. 1)
           -> Int       -- ^ radius of the localmin filter (e.g. 7)
           -> Float     -- ^ fraction of the maximum response allowed (e.g. 0.1)
           -> Int       -- ^ maximum number of interest points
           -> Img       -- ^ source image (I32f or Gray)
           -> IO [[[Double]]]  -- ^ result
-}

{-
getSaddlePoints' s r p mx im@Img{itype = Gray} =
    scale8u32f 0 1 im >>= getSaddlePoints s r p mx

getSaddlePoints' smooth rad prop maxn im@Img{itype = I32f} = do
    let suaviza = smooth `times` gauss Mask5x5
    gs <- suaviza im >>= secondOrder
    h <- hessian gs >>= scale32f (-1.0)
    (mn,mx) <- minmax h
    hotPoints <- localMax rad h
              >>= thresholdVal32f (mx*prop) 0.0 IppCmpLess
              >>= getPoints32f maxn
    let (gx,gy,gxx,gyy,gxy) = gs
    let f iml [r,c] = mapM (val32f' r c) iml where val32f' r c im = val32f im r c
    let g = f [h,im,gx,gy,gxx,gyy,gxy]
    r <- mapM g hotPoints
    let r' = map (map (fromRational.toRational)) r
    let norpts = pixelToPoint im $ map (reverse . map fromIntegral) hotPoints
    let dats = map taylor $ zipWith (++) norpts r'
    let dets = map head dats
    --print [[h+dt]| [h,dt,l1,l2]<- dets]
    return $ concat (map tail dats)
-}

taylor' [x,y,h,g,gx,gy,gxx,gyy,gxy] = [[h,dt,l1,l2],[x0,y0]{-,[x1,y1],[x0,y0]-},[x2,y2]] where
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
    signscale = 0.05 * signum (gx*v1x - gy*v1y)
    (dx,dy,dx',dy') = if l1>0 then (v1x,v1y,v2x,v2y) else (v2x,v2y,v1x,v1y)
    x0 = x
    y0 = y
    x1 = x0+signscale*dx
    y1 = y0+signscale*dy
    x2 = x0+0.2*gx
    y2 = y0-0.2*gy
    --x2 = x0+signscale*dx'
    --y2 = y0+signscale*dy'

cDF = fromRational.toRational

taylor x y gx gy gxx gyy gxy = (sp,vp) where
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
    vp = (v1x,v1y)
    sp = [x {- -cDF ((gyy*gx-gxy*gy)/dt)-}, y {- -cDF((gxy*gx-gxx*gy)/dt)-}]

getSaddlePoints s r p mx fn fr im@Img{itype = Gray} =
    scale8u32f 0 1 im >>= getSaddlePoints s r p mx fn fr

getSaddlePoints smooth rad prop maxn fn fr im@Img{itype = I32f} = do
    let suaviza = smooth `times` gauss Mask5x5
    sm <- suaviza im
    gs <- secondOrder sm
    h <- hessian gs >>= scale32f (-1.0)
    (mn,mx) <- minmax h
    hotPoints <- localMax rad h
              >>= thresholdVal32f (mx*prop) 0.0 IppCmpLess
              >>= getPoints32f maxn

    let (gx,gy,gxx,gyy,gxy) = gs

    let hp = map (reverse . map fromIntegral) hotPoints

    let loc = zip hotPoints hp

    let dir ([r,c],[x,y]) = do
        dx <- val32f gx r c
        dy <- val32f gy r c
        dxx <- val32f gxx r c
        dyy <- val32f gyy r c
        dxy <- val32f gxy r c
        let (sp,(vx,vy)) = taylor x y dx dy dxx dyy dxy
        let sg = signum (dx*vx - dy*vy)
        return (sp,atan2 (sg*vy) (sg*vx))

    info <- mapM dir loc
    let dirs = map snd info
    let sp = map fst info
    let cs = zipWith (circle fn fr) hotPoints dirs
    feats' <- mapM (extractList sm) cs
    let feats = map (map (fromRational.toRational)) feats'

    let r = zipWith5 IP sp (pixelToPoint im sp) (map (fromRational.toRational) dirs) feats (repeat 0)

    return r

extractList im l = mapM f l where
    f (r,c) = val32f im r c

circle :: Int -> Float -> [Int] -> Float -> [(Int,Int)]
circle n rad [r,c] ang = cir where
    d = 2*pi/fromIntegral n
    cir = [(round $ fromIntegral r-rad*sin (ang+k),
            round $ fromIntegral c+rad*cos (ang+k))| k <- [0, d .. 2*pi-d]]

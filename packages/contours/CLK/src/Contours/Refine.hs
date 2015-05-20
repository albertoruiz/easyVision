module Contours.Refine (
    prepro, normalizedXORError,
    refineH, refineP, refinePB, refinePGen, initPoseEV, initPosePnP, initBartoli
)where

import Contours
import Contours.CLK
import Contours.CLKP
import Vision.Ippe
import Numeric.LinearAlgebra.HMatrix
import Vision(cameraFromHomogZ0,kgen,refineNewton)
import Util.Geometry(datMat,segmentLength)
import Control.Arrow((&&&))
import Util.Misc(stdpix)
import OpenCV

type M = Matrix Double

prepro :: Polyline -> Maybe Shape
prepro = fmap andMore . shape 10
  where
    andMore s = s {gRefiner = \n -> if n>0 then (lkdHomography &&& (head . lkdErrors)) . refineHN (n-1) (shapeContour s)
                                           else (,) (ident 3) . last . lkdErrors . refineHN 0 (shapeContour s)
                  }


refineH :: Int -> Int -> ShapeMatch -> M
refineH k1 k2 r = h
  where
    prep@(f0,_j0,ffeat) = shapeGNS (proto r)   -- or shapeGNP
    hInit = wh r
    obs = shapeContour (target r)
    iniTarget = transPol (inv hInit) obs
    err0 = ffeat iniTarget - f0
    (tH,rHomog,_,_errProj) = iterate (stepGN' prep) (iniTarget,hInit,err0, norm_2 err0) !! k1

    (hxor,_errxor) = gRefiner (proto r) k2 tH
    h = rHomog <> inv hxor


type InitPose = Double -> ShapeMatch -> M -> M

refinePGen :: InitPose -> Int -> Int -> Double -> ShapeMatch -> M -> M
refinePGen initFun nint k4 f r h = p
  where
    obs = shapeContour (target r)

    rPose = initFun f r h

    ik = kgen (1/f)
    c0 = ik <> rPose ¿ [0,1,3]
    norobs = transPol ik obs

    rs  | nint <= 50 = preSamp
        | otherwise  = customSamp
      where
        preSamp    = rsp (proto r)
        customSamp = Closed $ resample nint (shapeContour $ proto r)

    refine c = poseStep rs c norobs

    p = kgen f <> fillr3 ((iterate refine c0)!!k4)


initPoseEV :: Int -> InitPose
initPoseEV k3 f r h = p
  where
    mbp0 = cameraFromHomogZ0 (Just f) h
    p = case mbp0 of
              Just p0 -> refineNewton k3 p0 views points
              Nothing     -> kgen f
       where
         views  = rspm3 (proto r) <> tr h
         points = rspm4 (proto r)


initPosePnP :: InitPose
initPosePnP f r h = solvePNP (kgen f) views points
  where
    model = shapeContour (proto r)
    pproj  = transPol h prs
    prs    = Closed (resample 10 model)
    views  = datMat (polyPts pproj)
    points = datMat (polyPts prs) ¦ 0


initBartoli :: InitPose
initBartoli f _ h = k <> (fst . h2p) (ik <> h)
  where
    k  = kgen f
    ik = kgen (1/f)


refineP :: Int -> Double -> ShapeMatch -> M -> M
refineP = refinePGen initPosePnP 50

refinePB :: Int -> Double -> ShapeMatch -> M -> M
refinePB = refinePGen initBartoli 1000


fillr3 :: Matrix Double -> Matrix Double
fillr3 m = fromColumns [r1,r2,r3,t]
      where
        [r1,r2,t] = toColumns m
        r3 = cross r1 r2

normalizedXORError :: Polyline -> Polyline -> Double
normalizedXORError predicted observed
    | ok = f3 $ f1 (diffCont predicted observed) / perimeter observed / stdpix
    | otherwise = 1000
  where
    g (s,e) = abs e * segmentLength s
    f1 = sum . map g
    f3 0 = 1000000
    f3 x = x

    lpred = perimeter predicted
    lobs  = perimeter observed
    ok = abs (lpred-lobs)/lobs < 0.25  -- FIXME needed now?


module Contours.Refine (
    prepro, normalizedXORError,
    refineH, refineP
)where

import Contours
import Contours.CLK
import Contours.CLKP
import Numeric.LinearAlgebra.HMatrix
--import Vision(cameraFromHomogZ0,kgen,refineNewton)
import Vision(kgen)
import Util.Geometry(datMat,segmentLength)
import Control.Arrow((&&&))
import Util.Misc(stdpix)
import OpenCV


prepro :: Polyline -> Maybe Shape
prepro = fmap andMore . shape 10
  where
    andMore s = s {gRefiner = \n -> if n>0 then (lkdHomography &&& (head . lkdErrors)) . refineHN (n-1) (shapeContour s)
                                           else (,) (ident 3) . last . lkdErrors . refineHN 0 (shapeContour s)
                  }


refineH :: Int -> Int -> ShapeMatch -> Matrix Double
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


refineP :: Int -> Int -> Double -> ShapeMatch -> Matrix Double -> Matrix Double
refineP k3 k4 f r h = p
  where
    model = shapeContour (proto r)
    obs = shapeContour (target r)
{-
    mbpose0 = cameraFromHomogZ0 (Just f) h
    rPose' = case mbpose0 of
              Just rPose0 -> refineNewton k3 rPose0 views points
              Nothing     -> kgen f
       where
         views  = rspm3 (proto r) <> tr h
         points = rspm4 (proto r)
-}
    rPose = solvePNP (kgen f) views points
      where
        pproj  = transPol h prs
        prs    = Closed (resample 10 model)
        views  = datMat (polyPts pproj)
        points = datMat (polyPts prs) ¦ 0


    ik = kgen (1/f)

    c0 = ik <> rPose ¿ [0,1,3]
    norobs = transPol ik obs
    p = kgen f <> fillr3 ((iterate refine c0)!!k4)
      where
        refine c = poseStep rs c norobs
        rs = rsp (proto r)


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
    ok = abs (lpred-lobs)/lobs < 0.25


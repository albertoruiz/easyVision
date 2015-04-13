{-# LANGUAGE RecordWildCards #-}

module Contours.Matching(
    Shape(..), ShapeMatch(..),
    shape, shapeMatches, shapeMatch,
    elongated, isEllipse,
    GN, prepareGNS, stepGN, stepGN'
) where

import Control.Arrow((&&&))
import Numeric.LinearAlgebra.HMatrix
import Data.List(minimumBy)
import Util.Misc(Mat,Vec,angleDiff)
import Util.Debug(assert)
import Util.Rotation
import qualified Util.Homogeneous as H
import Contours.Base
import Contours.Normalization
import Contours.Fourier
import Contours.Orientation
import Data.Function(on)
import Contours.GNS
import Contours.Resample
import Util.Geometry(datMat)


shape :: Int -> Polyline -> Maybe Shape
shape n = analyzeShape n . (id &&& momentsContour . polyPts)

----------------------------------------------------------------------
type CVec = Vector (Complex Double)

data Shape = Shape { shapeContour  :: Polyline
                   , shapeMoments  :: (Double,Double,Double,Double,Double)
                   , shapeCenter   :: Point
                   , shapeAxes     :: (Double,Double,Double)
                   , shapeWhitener :: Mat
                   , wfou          :: Int -> Complex Double
                   , whiteShape    :: Polyline
                   , invAffine     :: Vec
                   , invSimil      :: Vec
                   , invKS         :: Vec
                   , symmet2       :: Double
                   , symmet4       :: Double
                   , symmet0       :: Double
                   , kAngles       :: [Double]
                   , kFeats        :: [CVec]
                   , kHyps         :: [(CVec,(Mat,Double))]
                   , kShapes       :: [Polyline]
                   , shapeGNS      :: GN
                   , shapeGNP      :: GN
                   , gRefiner      :: Int -> Polyline -> (Mat,Double)
                   , wj0, wj0i     :: Mat
                   , wf0           :: Vec
                   , rspm3,rspm4   :: Mat
                   , rsp           :: Polyline
                   }


analyzeShape :: Int -> (Polyline, (Double, Double, Double, Double, Double)) -> Maybe Shape
analyzeShape mW (p,(mx,my,cxx,cyy,cxy))
    | l2 > 0 && l2 > l1 / 1000**2 = Just (Shape {..})
    | otherwise = Nothing
  where
    shapeContour = p
    shapeMoments = (mx,my,cxx,cyy,cxy)
    shapeCenter = Point mx my
    shapeAxes @ (l1,l2,phi) = eig2x2Dir (cxx,cyy,cxy)
    shapeWhitener = diagl [1/sqrt l1,1/sqrt l2,1] <> rot3 phi <> H.desp (-mx,-my)
    whiteShape = transPol shapeWhitener p
    fou = fourierPL whiteShape
    wfou = fou
    invAffine = fromList $ map (magnitude.fou) [-mW .. mW]

--  kAngles = icaAngles whiteShape >>= (\a -> [a,a+pi])
    (invKS,kAngles) = impang whiteShape
    
    kFeats = map f kAngles
      where
        f a = g $ normalizeStart $ (*cis (-a)) . fou
        g fun = fromList $ map fun [-mW .. mW]
    kws = map rot3 kAngles
    kHyps = zip kFeats (zip kws kAngles)
    kShapes = map (flip transPol whiteShape) kws
    
    invSimil = 2*featNotBad
    fouSimil = magnitude . fourierPL p

    featNotBad = fromList [ f k / s | k <- [-mW,-mW+1 .. -2] ++ [2..mW]]
       where
         f = fouSimil
         s = f 1 + f (-1)

    impang wshape = (feat, ias)
      where
        (as,ks) = unzip (anglesKurt wshape)

        feat = fromList $ map (subtract (-25.1)) (take 2 ks) ++ [abs (da/degree - 90)]
        ias = take 4 as >>= (\a->[a,a+pi])

        da = angleDiff (as!!0) (as!!1)

    f1:f2:f3:_ = kFeats
    symmet2 = norm_2 (f1-f2)
    symmet4 = norm_2 (f2-f3)
    symmet0 = norm_2 (f1 - fromList (replicate (mW+1) 0 ++ 2: replicate (mW-1) 0))
    
    shapeGNS = prepareGNS (1+4*3) p
    shapeGNP = prepareGNP 20 p
    
    gRefiner = undefined
    
    (wf0,wj0,_) = prepareGNS (1+4*3) whiteShape
    wj0i = (tr wj0 <> wj0) <\> (tr wj0)
    
    prs = Closed (resample 10 p)
    rspm4 = (datMat (polyPts prs) ¦ 0) ¦ 1
    rspm3 = rspm4 ¿ [0,1,3]
    
    rsp = Closed (resample 50 p)

----------------------------------------------------------------------

data ShapeMatch = ShapeMatch
    { proto      :: Shape
    , label      :: String
    , target     :: Shape
    , invDist    :: Double
    , alignDist  :: Double
    , wt, wp, wa :: Mat
    , wh         :: Mat
    , waRot      :: Double
    }


rotTrans :: Mat -> Double        
rotTrans w = rho
  where
    [[a1,a2],[b1,b2],[_c1,_c2]] = H.ht w [[0,0],[0,1],[1,0]]
    dx = b1-a1
    dy = b2-a2
    rho = atan2 dx dy


shapeMatches :: [(Shape,String)] -> Shape -> [ShapeMatch]
shapeMatches prots c = concatMap (sm c) prots
  where
    sm x p | asym p = [match r0 x p]
           | asym4 p = [match r0 x p, match r1 x p]
           | otherwise = [match (rot3 (k*pi/2)) x p | k <-[0..3] ]
    asym  (y,_) = 0.2 < symmet2 y
    asym4 (y,_) = 0.2 < symmet4 y
    r0 = ident 3
    r1 = rot3 pi
    match r x (y,l) = ShapeMatch {..}
      where
        proto = y
        label = l
        target = x
        invDist = (dist `on` invAffine) x y
        dist a b = norm_2 (a-b)
        (alignDist,((_ft,(wt,at)),(_fp,(wp,ap)))) = minimumBy (compare `on` fst) $
            [ (d ht hp, (ht,hp)) | hp <- take 4 $ kHyps proto, ht <- take 1 $ kHyps target]
        d (u,_) (v,_) = norm_2 (u-v)
        wa = inv (wt <> shapeWhitener target) <> r <> wp <> shapeWhitener proto
        waRot = rotTrans wa
        wh = inv (wt <> shapeWhitener target) <> r <> wp <> dh <> shapeWhitener proto

        err   = fromList (map (featF (ap-at) (wfou target)) [0..13]) - wf0 proto
        delta = wj0i proto #> err
        dh = mktP (toList delta)

---------------------------------------------------------------------

shapeMatch :: [(Shape,String)] -> Shape -> [ShapeMatch]
shapeMatch prots c = map (match c) prots
  where
    match x (y,l) = ShapeMatch {..}
      where
        proto = y
        label = l
        target = x
        invDist = (dist `on` invAffine) x y
        dist a b = norm_2 (a-b)
        (alignDist,((_ft,(wt,at)),(_fp,(wp,ap)))) = minimumBy (compare `on` fst) $
            [ (d ht hp, (ht,hp)) | hp <- take 4 $ kHyps proto, ht <- take 1 $ kHyps target]
        d (u,_) (v,_) = norm_2 (u-v)

        wa = inv (wt <> shapeWhitener target) <> wp <> shapeWhitener proto
        waRot = rotTrans wa
        wh = inv (wt <> shapeWhitener target) <> wp <> dh <> shapeWhitener proto

        err   = fromList (map (featF (ap-at) (wfou target)) [0..13]) - wf0 proto
        delta = wj0i proto #> err
        dh = mktP (toList delta)

----------------------------------------------------------------------  

-- | checks if a polyline is very similar to an ellipse.
isEllipse :: Int -- ^ tolerance (per 1000 of total energy) (e.g. 10)
          -> Polyline -> Bool
isEllipse tol c = (ft-f1)/ft < fromIntegral tol/1000 where
    wc = whitenContour c   -- required?
    f  = fourierPL wc
    f0 = magnitude (f 0)
    f1 = sqrt (magnitude (f (-1)) ^(2::Int) + magnitude (f 1) ^(2::Int))
    ft = sqrt (norm2Cont wc - f0 ^(2::Int))

----------------------------------------------------------------------

elongated :: Double -> Shape -> Bool
elongated r Shape { shapeAxes = (l1,l2,_) } = assert (l1 > thl1) "l1 very small in elongated"
                                            $ l2 / l1 < 1/r**2
  where
    thl1 = (1 * 2/640)**2

--------------------------------------------------------------------------------


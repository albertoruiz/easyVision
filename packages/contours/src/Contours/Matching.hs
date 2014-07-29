{-# LANGUAGE RecordWildCards #-}

module Contours.Matching(
    Shape(..), ShapeMatch(..),
    shape, shapeMatch, shapeMatches,
    elongated, isEllipse, stepGNS
) where

import Control.Arrow((***),(&&&))
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util(norm,diagl)
import Text.Printf(printf)
import Data.List(minimumBy,sortBy,groupBy)
import Util.Misc(Mat,Vec,degree,posMax,angleDiff)
import Util.Debug(debug,assert,warning)
import Util.Rotation
--import Classifier(Sample)
import Util.Homogeneous(ht,desp)
import Util.Options(optionFromFile)
import Contours.Base
import Contours.Normalization
import Contours.Fourier
import Contours.Orientation
import Control.Monad(when)
import Control.Applicative((<$>))
import Data.Maybe(isJust)
import Data.Function(on)
import Contours.GNS


shape :: Int -> Polyline -> Maybe Shape
shape n = analyzeShape n . (id &&& momentsContour . polyPts)

----------------------------------------------------------------------
type CVec = Vector (Complex Double)

data Shape = Shape { shapeContour  :: Polyline
                   , shapeMoments  :: (Double,Double,Double,Double,Double)
                   , shapeCenter   :: Point
                   , shapeAxes     :: (Double,Double,Double)
                   , shapeWhitener :: Mat
                   , whiteShape    :: Polyline
                   , invAffine     :: Vec
                   , invSimil      :: Vec
                   , invKS         :: Vec
                   , symmet2       :: Double
                   , symmet4       :: Double
                   , symmet0       :: Double
                   , kAngles       :: [Double]
                   , kFeats        :: [CVec]
                   , kHyps         :: [(CVec,Mat)]
                   , kShapes       :: [Polyline]
                   , kFeatsMirror  :: [CVec]
                   , kHypsMirror   :: [(CVec,Mat)]
                   , shapeGN       :: GN
                   }


analyzeShape mW (p,(mx,my,cxx,cyy,cxy))
    | l2 > 0 && l2 > l1 / 1000**2 = Just (Shape {..})
    | otherwise = Nothing
  where
    shapeContour = p
    shapeMoments = (mx,my,cxx,cyy,cxy)
    shapeCenter = Point mx my
    shapeAxes @ (l1,l2,phi) = eig2x2Dir (cxx,cyy,cxy)
    shapeWhitener = diagl [1/sqrt l1,1/sqrt l2,1] <> rot3 phi <> desp (-mx,-my)
    whiteShape = transPol shapeWhitener p
    fou = fourierPL whiteShape
    invAffine = fromList $ map (magnitude.fou) [-mW .. mW]

--  kAngles = icaAngles whiteShape >>= (\a -> [a,a+pi])
    (invKS,kAngles) = impang whiteShape
    
    kFeats = map f kAngles
      where
        f a = g $ normalizeStart $ (*cis (-a)) . fou
        g fun = fromList $ map fun [-mW .. mW]
    kws = map rot3 kAngles
    kHyps = zip kFeats kws
    kShapes = map (flip transPol whiteShape) kws
    
    kFeatsMirror = kFeats ++ map conj kFeats
    kHypsMirror = kHyps ++ map (conj *** (*diagl[1,-1,1])) kHyps

    invSimil = 2*featNotBad
    fouSimil = magnitude . fourierPL p
    featBad = fromList [ f k / f 1 | k <- [ 2 .. mW ]]
       where
         f k = fouSimil k + fouSimil (-k)
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
    symmet2 = pnorm PNorm2 (f1-f2)
    symmet4 = pnorm PNorm2 (f2-f3)
    symmet0 = pnorm PNorm2 (f1 - fromList (replicate (mW+1) 0 ++ 2: replicate (mW-1) 0))
    
    shapeGN = prepareGNS (1+4*3) p
    
----------------------------------------------------------------------

data ShapeMatch = ShapeMatch
    { proto      :: Shape
    , label      :: String
    , target     :: Shape
    , invDist    :: Double
    , ksDist     :: Double
    , alignDist  :: Double
    , wt, wp, wa :: Mat
    , waRot      :: Double
    }

shapeMatch :: [(Shape,String)] -> Shape -> [ShapeMatch]
shapeMatch prots c = map (match c) prots
  where
    match x (y,l) = ShapeMatch {..}
      where
        proto = y
        label = l
        target = x
        invDist = (dist `on` invAffine) x y
        ksDist = (dist `on` invKS) x y
        dist a b = norm (a-b)
        (alignDist,((ft,wt),(fp,wp))) = minimumBy (compare `on` fst)
            [ (d ht hp, (ht,hp)) | hp <- take 8 (kHyps proto), ht <- kHyps target]
        d (u,_) (v,_) = pnorm PNorm2 (u-v)
        wa = inv (wt <> shapeWhitener target) <> wp <> shapeWhitener proto
        waRot = rotTrans wa
        
        
rotTrans w = rho
  where
    [[a1,a2],[b1,b2],[c1,c2]] = ht w [[0,0],[0,1],[1,0]]
    dx = b1-a1
    dy = b2-a2
    d  = max eps $ sqrt $ abs (dx*dx + dy*dy)
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
    r2 = rot3 (pi/2)
    match r x (y,l) = ShapeMatch {..}
      where
        proto = y
        label = l
        target = x
        invDist = (dist `on` invAffine) x y
        ksDist = (dist `on` invKS) x y
        dist a b = norm (a-b)
        (alignDist,((ft,wt),(fp,wp))) = minimumBy (compare `on` fst)
            [ (d ht hp, (ht,hp)) | hp <- kHyps proto, ht <- kHyps target]
        d (u,_) (v,_) = pnorm PNorm2 (u-v)
        wa = inv (wt <> shapeWhitener target) <> r <> wp <> shapeWhitener proto
        waRot = rotTrans wa

----------------------------------------------------------------------  

-- | checks if a polyline is very similar to an ellipse.
isEllipse :: Int -- ^ tolerance (per 1000 of total energy) (e.g. 10)
          -> Polyline -> Bool
isEllipse tol c = (ft-f1)/ft < fromIntegral tol/1000 where
    wc = whitenContour c   -- required?
    f  = fourierPL wc
    f0 = magnitude (f 0)
    f1 = sqrt (magnitude (f (-1)) ^2 + magnitude (f 1) ^2)
    ft = sqrt (norm2Cont wc - f0 ^2)

----------------------------------------------------------------------

elongated r Shape { shapeAxes = (l1,l2,_) } = assert (l1 > thl1) "l1 very small in elongated"
                                            $ l2 / l1 < 1/r**2
  where
    thl1 = (1 * 2/640)**2

--------------------------------------------------------------------------------


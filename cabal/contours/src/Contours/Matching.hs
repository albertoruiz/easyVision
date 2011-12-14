{-# LANGUAGE RecordWildCards #-}

module Contours.Matching(
    Shape(..), ShapeMatch(..),
    shape,
    elongated, isEllipse,
    matchShapes, matchShapesSimple
) where

import Control.Arrow((***),(&&&))
import Numeric.LinearAlgebra
import Text.Printf(printf)
import Data.List(minimumBy,sortBy,groupBy)
import Util.Misc(Mat,Vec,norm,degree,diagl,debug,posMax,norm)
import Util.Rotation
import Classifier(Sample)
import Vision
import ImagProc.Base(Polyline(..))
import Util.Options(optionFromFile)
import Contours.Base
import Contours.Normalization
import Contours.Fourier
import Contours.Orientation
import Control.Monad(when)
import Control.Applicative((<$>))
import Data.Maybe(isJust)
import Data.Function(on)


shape :: Polyline -> Shape
shape = analyzeShape 10 . (id &&& momentsContour . polyPts)

----------------------------------------------------------------------
type CVec = Vector (Complex Double)

data Shape = Shape { shapeContour  :: Polyline
                   , shapeMoments  :: (Double,Double,Double,Double,Double)
                   , shapeAxes     :: (Double,Double,Double)
                   , shapeWhitener :: Mat
                   , whiteShape    :: Polyline
                   , invAffine     :: Vec
                   , invSimil      :: Vec
                   , kAngles       :: [Double]
                   , kFeats        :: [CVec]
                   , kHyps         :: [(CVec,Mat)]
                   , kShapes       :: [Polyline]
                   , kFeatsMirror  :: [CVec]
                   , kHypsMirror   :: [(CVec,Mat)]
                   }


analyzeShape mW (p,(mx,my,cxx,cyy,cxy)) = Shape {..}
  where
    shapeContour = p
    shapeMoments = (mx,my,cxx,cyy,cxy)
    shapeAxes @ (l1,l2,phi) = eig2x2Dir (cxx,cyy,cxy)
    shapeWhitener = rot3 (27*degree) <> diagl [1/sqrt l1,1/sqrt l2,1] <> rot3 phi <> desp (-mx,-my)
    whiteShape = transPol shapeWhitener p
    fou = fourierPL whiteShape
    invAffine = fromList $ map (magnitude.fou) [-mW .. mW]

    kAngles = icaAngles whiteShape >>= (\a -> [a,a+pi])
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

----------------------------------------------------------------------

data ShapeMatch = ShapeMatch
    { proto      :: Shape
    , label      :: String
    , target     :: Shape
    , invDist    :: Double
    , alignDist  :: Double
    , wt, wp, wa :: Mat
    , waRot      :: Double
    }

shapeMatch :: Sample Shape -> Shape -> [ShapeMatch]
shapeMatch prots c = map (match c) prots
  where
    match x (y,l) = ShapeMatch {..}
      where
        proto = y
        label = l
        target = x
        invDist = (dist `on` invAffine) x y
        dist a b = norm (a-b)    
        (alignDist,((ft,wt),(fp,wp))) = minimumBy (compare `on` fst) [ (d ht hp, (ht,hp)) | hp <- take 8 (kHyps proto), ht <- kHyps target]
        d (u,_) (v,_) = pnorm PNorm2 (u-v)
        wa = inv (wt <> shapeWhitener target) <> wp <> shapeWhitener proto
        waRot = rotTrans wa
        
rotTrans w = rho
  where
    [[a1,a2],[b1,b2]] = ht w [[0,0],[0,1]]
    dx = b1-a1
    dy = b2-a2
    rho = atan2 dx dy
  
----------------------------------------------------------------------  
  
matchShapes th1 th2 ((x,cs),prots) = (x, map (filterGood . shapeMatch prots) cs)
  where
    filterGood = sortBy (compare `on` alignDist) . filter good
    good m = invDist m < th1 && alignDist m < th2

matchShapesSimple th ((x,cs),prots) = (x, map (filterGood . shapeMatch prots) cs)
  where
    filterGood = sortBy (compare `on` invDist) . filter ((<th).invDist)

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

elongated r Shape { shapeAxes = (l1,l2,_) } = sqrt l2 / sqrt l1 < 1/r 


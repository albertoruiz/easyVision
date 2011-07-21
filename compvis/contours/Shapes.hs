{-# LANGUAGE RecordWildCards #-}

module Shapes(
    Shape(..), ShapeMatch(..),
    shape,
    elongated,
    matchShapes, matchShapesSimple
) where

import EasyVision
import Control.Arrow((***),(&&&))
import Graphics.UI.GLUT hiding (Point,Size)
import Data.Colour.Names
import Numeric.LinearAlgebra
import Text.Printf(printf)
import Data.List(minimumBy,sortBy,groupBy)
import Util.Misc(Mat,Vec,norm,degree,diagl,debug,posMax,norm)
import Util.Rotation
import Classifier(Sample)
import Vision
import Util.Options(optionFromFile)

import Control.Monad(when)
import Control.Applicative((<$>))
import Data.Maybe(isJust)


shape :: Polyline -> Shape
shape = analyzeShape 10 . (id &&& momentsContour . polyPts)

elongated r Shape { shapeAxes = (l1,l2,_) } = sqrt l2 / sqrt l1 < 1/r 

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

data ShapeMatch = ShapeMatch {
    proto  :: Shape,
    label  :: String, 
    target :: Shape,
    invDist :: Double,
    wt, wp, wa :: Mat,
    alignDist :: Double }

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
        (alignDist,((ft,wt),(fp,wp))) = minimumBy (compare `on` fst) [ (d ht hp, (ht,hp)) | hp <- kHyps proto, ht <- kHyps target]
        d (u,_) (v,_) = pnorm PNorm2 (u-v)
        wa = inv (wt <> shapeWhitener target) <> wp <> shapeWhitener proto


matchShapes th1 th2 ((x,cs),prots) = (x, map (filterGood . shapeMatch prots) cs)
  where
    filterGood = sortBy (compare `on` alignDist) . filter good
    good m = invDist m < th1 && alignDist m < th2

matchShapesSimple ((x,cs),prots) = (x, map (filterGood . shapeMatch prots) cs)
  where
    filterGood = sortBy (compare `on` invDist) . filter ((<0.3).invDist)


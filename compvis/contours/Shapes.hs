{-# LANGUAGE TemplateHaskell, RecordWildCards, NamedFieldPuns #-}

module Shapes(
    Shape(..),
    shape,
    elongated,
    preClassify
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
                   , fourier       :: Int -> Complex Double
                   , featInv       :: Vec
                   , invSimil      :: Vec
                   , kAngles       :: [Double]
                   }


analyzeShape mW (p,(mx,my,cxx,cyy,cxy)) = Shape {
    shapeContour = p,
    shapeMoments = (mx,my,cxx,cyy,cxy),
    shapeAxes = (l1,l2,phi),
    shapeWhitener = w,
    whiteShape = r,
    fourier = normalizeStart fou,
    featInv = fromList $ map (magnitude.fou) [-mW .. mW],
    invSimil = featNotBad,
    kAngles = as }
  where
    (l1,l2,phi) = eig2x2Dir (cxx,cyy,cxy)
    w = rot3 (27*degree) <> diagl [1/sqrt l1,1/sqrt l2,1] <> rot3 phi <> desp (-mx,-my)
    r = transPol w p
    as = icaAngles r    
    fou = fourierPL r
    fouSimil = magnitude . fourierPL p
    featBad = fromList [ f k / f 1 | k <- [ 2 .. mW ]]
       where
         f k = fouSimil k + fouSimil (-k)
    featNotBad = fromList [ f k / s | k <- [-mW,-mW+1 .. -2] ++ [2..mW]]
       where
         f = fouSimil
         s = f 1 + f (-1)

----------------------------------------------------------------------

preClassify ((x,cs),prots) = (x, classifInvar prots cs)

classifInvar :: Sample Shape -> [Shape] -> [[((Double, String), (Shape, Shape))]]
classifInvar prots cs = oks
  where
    theDist = fst.fst
    oks =  map (sortBy (compare `on` theDist) . filter ((<0.2).theDist) . clas) cs
    clas x = map (basicDist x) prots
    basicDist x (y,l) = ((d,l),(x,y))
      where
        d = (dist `on` featInv) x y
--        d = (dist `on` invSimil) x y


dist a b = norm (a-b)

----------------------------------------------------------------------


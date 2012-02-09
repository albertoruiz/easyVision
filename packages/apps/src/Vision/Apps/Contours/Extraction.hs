{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

module Vision.Apps.Contours.Extraction (
    localOtsuContours, autoContoursOtsu,
    adaptiveThreshold, autoContours, ATParam(..),
) where

import Vision.GUI
import ImagProc
import Contours
import Control.Arrow((***),(&&&))
import Control.Applicative
import Numeric.LinearAlgebra((<>),fromList,inv)
import Text.Printf(printf)
import Util.Misc(diagl,mean,vec,debug,degree)
import Util.Rotation(rot3)
import Util.Options
import Vision(desp,inHomog,hv2pt)
import Classifier(Sample)
import Control.Monad(when)
import Data.List(minimumBy,sortBy)
import Data.Function(on)

--------------------------------------------------------------------------------

localOtsuContours :: ImageGray -> (([Polyline],[Polyline]),[Polyline])
localOtsuContours g = ((concatMap subcont subims,[]),[])
  where
    regs = filter (big.roiSize) $ map region $ map lst3 $ contours' 1000 30 128 True sal
      where
        region = shrink (-10,-10) . poly2roi (size g) . roi2poly (size sal)

    lst3 (_,_,a) = a
    fst3 (a,_,_) = a
    big (Size h w) = w > 32 && h > 32 
    gp = g
    b = filterBox8u 5 5 gp    
    sal = compare8u IppCmpGreaterEq b gp

    subims = map (\r -> setROI r g) regs
    
    subcont x = map (Closed . pixelsToPoints (size g) . fst3)
              $ contours' 3 100 128 False
              $ compareC8u (otsuThreshold x) IppCmpGreaterEq x


autoContoursOtsu :: ImageGray -> [Polyline]
autoContoursOtsu = localOtsuContours >>> map reducePolyline . fst . fst

--------------------------------------------------------------------------------

autoParam "ATParam" "at-"  [  ("radMed","Int",intParam 2 0 10)
                           ,  ("radBox","Int",intParam 10 1 20)
                           ,  ("scale","Float",realParam 2 0 10) ]

adaptiveThreshold ATParam{..} x = blk
  where
    pre  = filterMedian radMed x
    mfb  = scale .* (float pre |-| float (filterBox8u radBox radBox pre) )
    std  = (recip 255 .*) . rectStdDev radBox radBox . sqrIntegral $ pre
    enh = scale32f8u (-1) 1 (mfb |*| std)
    blk = compareC8u 127 IppCmpLess enh

autoContours :: ITrans ImageGray [Polyline]
autoContours = withParam adaptiveThreshold >>> arr (map reducePolyline . Contours.contours 1000 50)


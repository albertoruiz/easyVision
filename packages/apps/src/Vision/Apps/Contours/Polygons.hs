{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Vision.Apps.Contours.Polygons (
    polygonalize, PolygonParam(..),
    polygons, getFlats,  InfoPoint(..), extendedContour
) where

import Vision.GUI
import ImagProc
import Contours hiding (contours)
import Control.Arrow((***),(&&&))
import Control.Applicative
import Numeric.LinearAlgebra((<>),fromList,inv,(@>))
import Text.Printf(printf)
import Util.Misc(diagl,mean,vec,debug,degree,subListsBy,norm)
import Util.Rotation(rot3)
import Util.Options
import Vision(desp,inHomog,hv2pt,mseLine,cross)
import Classifier(Sample)
import Control.Monad(when)
import Data.List(minimumBy,sortBy,tails)
import Data.Function(on)


autoParam "PolygonParam" "polygon-"
  [("maxAng","Double" , realParam 5 0 10),
   ("maxCurv","Double",realParam 10 1 50),
   ("minSides","Int" , intParam 4 3 12),
   ("maxSides","Int", intParam 6 3 12) ]

instance ParamRecord PolygonParam where
    defParam = defPolygonParam
    argParam = argPolygonParam
    winParam = winPolygonParam

--------------------------------------------------------------------------------

polygonalize PolygonParam {..} = polygons maxCurv maxAng (minSides,maxSides)

--------------------------------------------------------------------------------

polygons :: Double -- ^ radius of curvature threshold (stdpix, e.g. 10)
         -> Double -- ^ max angle for joining (degrees, e.g. 5)
         -> (Int, Int) -- ^ min and max number of sides allowed
         -> [Polyline] -> [Polyline]
polygons mxrc mxang sds = selectPolygons sds . map (refinePolygon . getFlats mxrc mxang . extendedContour) 

selectPolygons :: (Int, Int) -- ^ min and max number of sides allowed
               -> [Polyline] -> [Polyline]
selectPolygons (mns,mxs) = filter ((`elem`[mns..mxs]) . length . polyPts) . sortBy (compare `on` (negate .area))

   
getFlats :: Double -- ^ radius of curvature threshold (stdpix, e.g. 10)
         -> Double -- ^ max angle for joining (degrees, e.g. 5)
         -> [InfoPoint] -> [[InfoPoint]]
getFlats mxrc mxang = regroup . filter ((>2). length) . subListsBy ((>mxrc*stdpix).radCurv)
  where
    regroup [] = []
    regroup [a] = [a]
    regroup (a:b:xs) = case ok of
        False -> a : regroup (b:xs)
        True  ->     regroup ((a++b):xs)
      where
        Point x0 y0 = pt (head a)
        Point x1 y1 = pt (last a)
        Point x0' y0' = pt (head b)
        Point x1' y1' = pt (last b)
        ux = x1-x0
        uy = y1-y0
        vx = x1'-x0'
        vy = y1'-y0'
        u2 = ux*ux+uy*uy
        v2 = vx*vx+vy*vy
        uv = ux*vx+uy*vy
        ca = uv/(sqrt (abs u2)*sqrt (abs v2))
        ok = abs ca > cos (mxang * degree)


stdpix = 2/640 :: Double

refinePolygon :: [[InfoPoint]] -> Polyline
refinePolygon = Closed . interPoints . map (fromList.mseLine.map pt) 
  where
    interPoints p = stp $ p ++ [head p]
      where
        stp [] = []
        stp [_] = []
        stp (a:b:rest) = inter a b : stp (b:rest)

        inter l1 l2 = hv2pt $ cross l1 l2


--------------------------------------------------------------------------------

data InfoPoint = InfoPoint { pt        :: Point
                           , vx, vy    :: Double
                           , radCurv      :: Double
                           }

--type ExtendedContour = [InfoPoint]

extendedContour :: Polyline -> [InfoPoint]
extendedContour (Closed ps) = mkInfo envs
  where
    envs = map (take 3) .  take (length ps) . tails $ ps++ps
extendedContour (Open ps) = mkInfo envs
  where
    envs = map (take 3) .  take (length ps-2) . tails $ ps

mkInfo :: [[Point]] -> [InfoPoint]
mkInfo envs = map curvat envs
  where
    curvat ps@[p0@(Point x0 y0),p1@(Point x1 y1),p2@(Point x2 y2)] = InfoPoint p1 wx wy k
      where
        l1 = bisector (Segment p0 p1)
        l2 = bisector (Segment p1 p2)
        hc = cross l1 l2
        tooflat = abs(hc@>2 /norm hc) < 1E-3
        cen@(Point kcx kcy) =  hv2pt hc
        k = distPoints cen p1 `min` 10
        vx = (x2-x0)/2
        vy = (y2-y0)/2

        cvx = y1-kcy
        cvy = -(x1-kcx)
        cv = cvx*cvx+cvy*cvy
        
        l = distPoints p1 p0 / sqrt (abs cv)

        (wx,wy) | True = (vx,vy)
                | otherwise = (cvx*l, cvy*l)


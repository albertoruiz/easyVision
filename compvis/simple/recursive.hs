import EasyVision
import Control.Arrow((***))
import Numeric.LinearAlgebra
import Data.List(sortBy)
import Vision
import Util.Misc(posMax)

main = run $ camera ~> grayscale
          >>= wcontours id ~> (id *** contSel)
          >>= polygonalize .@. winPolygonParam ~> snd
--          ~>  polygonalize defPolygonParam
          ~>  id *** sortBy (compare `on` negate.area)
          ~~> scanl1 rec
          >>= observe "recursive" fst
          >>= showPolygons
          >>= timeMonitor

----------------------------------------------------------------------

rec _ (im,[]) = (im,[])
rec (a,_) (im,c:_) = (warpOn im h a, [c])
  where
    h = estimateHomographyRaw (g c) [[1,r],[-1,r],[-1,-r],[1,-r]] <> scaling 1.01
    r = 0.75
    g (Closed ps) = map (\(Point x y) -> [x,y]) (up ps)
    up = rot (k+2)
    k = posMax $ map segmentLength (asSegments c)

----------------------------------------------------------------------

rot n xs = take (length xs) . drop n $  cycle xs

showPolygons = monitor "Polygon" (mpSize 10) sh
  where
    sh(im,cs) = do
        drawImage' im
        pointCoordinates (size im)
        mapM_ drawContourLabeled cs


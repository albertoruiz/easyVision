import Vision.GUI
import Image.Processing
import Util.Geometry
import Util.Polygon
import Contours
import Data.List(foldl1')

import Util.Homogeneous(desp,scaling)
import Numeric.LinearAlgebra(toList,linspace,(<>),uniformSample)
import Contours.Clipping


main = runIt $ do
    p <- clickPoints "click points" "--points" () sh
    w <- browser "Minkowski sum" [] (const id)
    connectWith g p w
  where
    g (k,_) ([],_) = (k, [])
    g (k,_) (ps,_) = (k, [ shms ps (pcircle 8 0.05)
                         , shms ps mask1
                         , shms ps mask2
                          ])

shms ps msk = Draw [ color white $ Closed ps
                   , color yellow $ unions $ minkowskiComponents msk (Polygon ps)
                   ]

mask = -- mask2 
       pcircle 5 0.1



sh ([],()) = Draw ()
sh (pts,()) = color white [ color yellow (map fillPolygon dil) 
                          , color lightgray (fillPolygon mink)
                          , color darkgray (fillPolygon (Polygon pts))
                          , color red (convexComponents (Polygon pts)) 
                  --        , color white $ Closed pts
                          , color white $ pointSz 1 $ drawPointsLabeled pmink
                  --        , color lightgray $ dil
                  --        , pointSz 3 $ color green $ pmink
                  --        , lineWd 5 $ color blue $ ((dil!!0) `u` (dil!!1)) -- `u` (dil!!2)
                  --        , lineWd 1 $ color green $ dil!!2 
                          , color green $ map f $ pmink
                          , color blue  $ map g pts
                  --        , lineWd 2 $ color green $ mink
--                          , color white $ lineWd 3 (Closed pts)
                          ]
  where
    f (Point x y) = hdesp x y <| mask
    g (Point x y) = hdespNeg x y <| mask
    dil = minkowskiComponents mask (Polygon pts)
    mink = Polygon . polyPts $ unions dil
    pmink = polygonNodes mink


hdesp x y = unsafeFromMatrix (desp (x,y)) :: Homography

hdespNeg x y = unsafeFromMatrix (desp (x,y) <> scaling (-1)) :: Homography

toPoly = Closed . polygonNodes

mask1 = Polygon [ Point (x-0.05) y, Point (x+0.05) y, Point x (y+0.1) ]
  where
   x = 0
   y = 0

mask2 = Polygon [Point (x-0.05) (y-0.05), Point x y, Point (x+0.05) (y-0.05), Point x (y+0.1-0.05) ]
  where
   x = 0
   y = 0

pcircle n r = Polygon [Point (r*cos t) (r*sin t) | t <- as]
  where
    as = init $ toList $ linspace (n+1) (0, 2*pi)

--------------------------------------------------------------------------------

perturb σ seed (Polygon ps) = Closed (zipWith f ps noise)
  where
    f (Point x y) (Point a b) = Point (x+a) (y+b)
    noise = unsafeMatDat (uniformSample seed (length ps) [(-σ,σ),(-σ,σ)])
     
union2 p1 p2 = uniquePts $ head $ clip ClipUnion p1 p2

unions = foldl1' union2 . zipWith (perturb 1E-6) [1000,1100 ..]

uniquePts :: Polyline -> Polyline
uniquePts = Closed . map f . filter ((> 0.001) . segmentLength) . asSegments
  where
    f (Segment _ p) = p
    


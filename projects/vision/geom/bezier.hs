import Vision.GUI.Simple
import Image
import Contours(asSegments)
import Util.Ellipses
import Util.Geometry
import Numeric.LinearAlgebra
import Util.Misc(debug)


main = runIt $ clickPoints "Bezier" "--points" () (sh.fst)

sh pts | length pts >= 3 = Draw 
            [ (color green . Open . drawBezier) d
        --  , (color gray . drawConic) c
            , pointSz 5 . color red $ d
            , drwpts
--            , pointSz 3 . color yellow $ map (bezierPoint d) ts
            ]
       | otherwise = drwpts
  where
    c = computeConic pts
    d = computeBezier pts
    drwpts = color white . drawPointsLabeled $ pts
    ts = debug "ts" id $ map (bezts d) (init . tail $ pts)

drawConic c = Draw ss
  where
    ps = pointsConic 50 c
    ss = filter ((1>).segmentLength) $ asSegments (Closed ps)


computeBezier ps = [p0, p1, p2]
  where
    p1 = Point x1 y1
    p0@(Point x0 y0) = head ps
    p2@(Point x2 y2) = last ps
    dists = scanl (+) 0 $ map segmentLength $ asSegments (Open ps)
    len = last dists
    pd = zip ps (map (/len) dists)
    eq (Point x y, t) = [ x-x0*(1-t)^2-x2*t^2
                        , y-y0*(1-t)^2-y2*t^2
                        , 2*t*(1-t) ]
    [cx,cy,cc] = toColumns $ fromLists $ map eq (init . tail $ pd)
    x1 = ( reshape 1 cc <\> cx ) @> 0
    y1 = ( reshape 1 cc <\> cy ) @> 0

drawBezier nodes = map (bezierPoint nodes) ts
  where
    ts = toList (linspace 100 (0,1))

bezierPoint [Point x0 y0, Point x1 y1, Point x2 y2] t = Point x y
  where
    x = (1-t)^2 * x0 + 2*t*(1-t)*x1 + t^2 * x2
    y = (1-t)^2 * y0 + 2*t*(1-t)*y1 + t^2 * y2


bezts [Point x0 y0, Point x1 y1, Point x2 y2] (Point x y) = (tx+ty)/2
  where
    tx = solve (eq x x0 x1 x2)
    ty = solve (eq y y0 y1 y2)
    eq z z0 z1 z2 = [z0-2*z1+z2, 2*(z1-z0), z0-z]
    solve [a, b, c] = head $ filter (<=1) $ filter (>=0) [s1,s2]
      where
        d = sqrt (b*b -4*a*c)
        s1 = (-b-d)/2/a
        s2 = (-b+d)/2/a
    

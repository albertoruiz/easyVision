import Vision.GUI
import ImagProc
import ImagProc.Camera
import Numeric.LinearAlgebra
import Util.Misc(degree,replaceAt,debug)

main = runIt $ splineWin

sp0 = (0,[(Point (-0.25 + 0.25*cos(pi/4)) (0+0.25*sin(pi/4)), 1, -pi/4),
          (Point 0 0,1, -pi/2),
          (Point (-0.25) (-0.25), 1, pi),
          (Point (-0.5) 0, 1, pi/2),
          (Point 0 0.5, 1, 0),
          (Point 0 (-0.5), 1, pi)])

splineWin = standalone (Size 400 400) "spline" sp0 updts [] sh
  where
    x0 = 7
    sh (k,seg) = Draw [ Draw (map drSeg (zip seg (tail seg))),
                        color blue, pointSz 3, points [fst3 $ seg!!k],
                        lineWd 1, color yellow, 
                        Draw $ circle (Point 0 0) 0.5,
                        Draw $ circle (Point (-0.25) 0) 0.25 ]      
    fst3(a,_,_) = a

    drSeg (s1@(p1@(Point x1 y1),v1,a1), s2@(p2@(Point x2 y2),v2,a2)) =
            Draw [ color white, pointSz 5, points [p1,p2],
                   color red, lineWd 1, dirs,
                   color white, Draw $ Open $ interpolate (s1,s2) ]
      where        
        vsc = 1/4
        d1x = x1+vsc*v1*cos a1
        d1y = y1+vsc*v1*sin a1
        d2x = x2+vsc*v2*cos a2
        d2y = y2+vsc*v2*sin a2
        dirs = Draw [ Draw (Open [p1, Point d1x d1y]),
                      Draw (Open [p2, Point d2x d2y]) ]

    updts = [ (key (SpecialKey KeyUp), \_ _ (k,seg)->((k+1) `mod` length seg, seg ))
            , (key (SpecialKey KeyDown), \_ _ (k,seg)->((k-1) `mod` length seg, seg))
            , (key (MouseButton WheelUp), changeV (*1.05) )
            , (key (MouseButton WheelDown), changeV (/1.05))
            , (key (MouseButton LeftButton), changepoint )
            , (kShift (key (MouseButton WheelUp)), changeA (+1*degree) )
            , (kShift (key (MouseButton WheelDown)), changeA (+(-1)*degree))]

modif f _roi pt (k,seg) = (k,replaceAt [k] [f pt (seg!!k)] seg)
changeA g = modif (\_ (p,v,a) -> (p,v, g a))
changepoint = modif (\pt (_,v,a) -> (pt,v,a))
changeV f _roi _pt (k,ss) = (k,map (\(p,v,a)->(p, f v,a)) ss)

data Spline = Spline { spA :: Double, spB :: Double, spC :: Double, spD :: Double }

spline x0 v0 x1 v1 = Spline a b c d
  where
    a = x0
    b = v0
    c = 3*(x1-x0) -2*v0 - v1
    d = v0+v1+2*x0-2*x1

mkSpline (Spline a b c d) = \t -> a + b*t + c*t**2 + d*t**3

interpolate ((p1@(Point x1 y1),v1,a1), (p2@(Point x2 y2),v2,a2)) =
    [Point (x t) (y t) | t <- toList (linspace 100 (0,1)) ]
  where
    x = mkSpline (spline (x1) (d*v1*cos a1) (x2) (d*v2*cos a2))
    y = mkSpline (spline (y1) (d*v1*sin a1) (y2) (d*v2*sin a2))
    d = distPoints p1 p2

circle (Point cx cy) r = Closed [Point (cx+r*cos t) (cy+r*sin t) | t <- toList (linspace 360 (0,359*degree)) ]


import Vision.GUI
import ImagProc
import Contours
import Vision
import Contours.Clipping
import Data.List(partition)

main = runIt win

win = browser "clipping" ds (const id)
    where
      ds = [ msg "original contours" [color blue a, color red b]
           , msg "diff a b" [color blue (clip ClipDifference a b)]
           , msg "diff b a" [color red (clip ClipDifference b a)]
           , msg "xor" [color blue (map fst zp), color red (map fst zn)]
           , msg "circ" $ map drCircuit (map circuit $ xorext a b)
           ]
           ++ map (msg "orig" . color blue . shOrig) zp
           ++ map (msg "orig" . color red  . shOrig) zn


a = Closed [Point x1 y1, Point 0 y1, Point x2 y1, Point x2 y2, Point x1 y2]
  where
    x1 = 0.6; x2 = -x1; y1 = 0.4; y2 = -y1

b = Closed [ Point x1 y2,  Point x1 0, Point x1 y1, Point x2 y1, Point x2 y2
           , Point x4 y2, Point x4 y3, Point x3 y3, Point x3 y2]
  where
    x1 = 0.4; x2 = -x1; x3 = 0.2; x4 = -x3;
    y1 = 0.8; y2 = -0.6; y3 = 0.6

(zp,zn) = partition ((<0).orientedArea.fst) (xorext a b)

msg s d = Draw [winTitle s, Draw d]


shOrig :: (Polyline, [Int]) -> Drawing
shOrig (p, os) = Draw [Draw p, color white $ drawThings (polyPts p) (zip [0..] os)]
  where
    drawThings pts xs = draws $ zipWith (textF Helvetica10) pts (map ((' ':).show) xs)

circuit (Closed ps, os) = (Closed ps, Open b)
  where
    [na,nb,nc] = map (\k -> length (filter (==k) os)) [1,2,3]
    b = [p | (p,k) <- zip ps os, k/=2 ]

drCircuit (p,b) = color c b
  where
    c | orientedArea p < 0 = blue
      | otherwise = red
    

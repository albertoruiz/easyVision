import Vision.GUI
import ImagProc
import Contours
import Vision
import Contours.Clipping
import Data.List(partition)
import Util.Misc(debug)

main = runIt win

win = browser "clipping" ds (const id)
    where
      ds = [ msg "original contours" [color blue a, color red b]
           ]
           ++ map (msg "positive" . color blue . shOrig) zp
           ++ map (msg "negative" . color red  . shOrig) zn


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

drawThings pts xs = draws $ zipWith (textF Helvetica10) pts (map ((' ':).show) xs)

{-

drCircuit (oa,b) = color c b
  where
    c | oa < 0 = blue
      | otherwise = red

circuit = step2 . step1

-- starting point and direction
step1 :: (Polyline, [Int]) -> (Double, [(Point, Int)])
step1 (ps, os) | oa < 0    = (oa, reverse $ rot $ h $ reverse pos)
               | otherwise = (oa, h pos)
  where
    pos = zip (polyPts ps) os
    oa = orientedArea ps
    h = until ((==3).snd.head) rot
    rot xs = tail xs ++ [head xs]

drStep1 (_,pos) = Draw [ Draw (Closed ps), drawThings ps (zip [0..] os) ]
  where
    (ps, os) = unzip pos

-- extract fragments
step2 :: (Double, [(Point, Int)]) -> (Double, [Segment])
step2 (oa, pos) = (oa, concatMap (asSegments . Open) (fragments pos))
  where
    fragments :: [(Point,Int)] -> [[Point]]
    fragments pos = map (map fst) $ frags $ filter ((/=1).snd) $ pos
      where
        frags [] = []
        frags (p:xs) = (p : rs ++ [q]) : frags ys
          where
            (rs,q:ys) = span ((==2).snd) xs
-}


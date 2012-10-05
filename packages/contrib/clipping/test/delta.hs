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
           , msg "XOR" [color blue (map (fst.fst) zp), color red (map (fst.fst) zn)]
           , msg "delta" (drawDelta deltas)
           , msg "delta" (drawDelta (deltaContour a b))
           ]
   --        ++ map (msg "positive" . color blue . shOrig) zp
   --        ++ map (msg "negative" . color red  . shOrig) zn


a' = Closed [Point x1 y1, Point 0 y1, Point x2 y1, Point x2 y2, Point x1 y2]
  where
    x1 = 0.6; x2 = -x1; y1 = 0.4; y2 = -y1

b' = Closed [ Point x1 y2,  Point x1 0, Point x1 y1, Point x2 y1, Point x2 y2
           , Point x4 y2, Point x4 y3, Point x3 y3, Point x3 y2]
  where
    x1 = 0.4; x2 = -x1; x3 = 0.2; x4 = -x3;
    y1 = 0.8; y2 = -0.6; y3 = 0.6


a'' = Closed [Point x1 y0, Point x8 y0, Point x8 y7, Point x6 y7, Point x6 y1
           , Point x4 y1, Point x4 y7, Point x3 y7, Point x3 y1, Point x2 y1
           , Point x2 y7, Point x1 y7 ]
  where
    x0 = 0.9; x1 = 0.8; x2 = 0.4; x3 = 0.2; x4 =  0.0; x5 = -0.2; x6 = -0.4; x7 = -0.6; x8 = -0.8; x9 = -0.9;
    y0 = 0.8; y1 = 0.4; y2 = 0.2; y3 = 0.0; y4 = -0.2; y5 = -0.4; y6 = -0.6; y7 = -0.8;

b'' = Closed [Point x0 y2, Point x9 y2, Point x9 y6, Point x0 y6, Point x0 y5 
           , Point x7 y5, Point x7 y4, Point x5 y4, Point x5 y3, Point x0 y3]
  where
    x0 = 0.9; x1 = 0.8; x2 = 0.4; x3 = 0.2; x4 =  0.0; x5 = -0.2; x6 = -0.4; x7 = -0.6; x8 = -0.8; x9 = -0.9;
    y0 = 0.8; y1 = 0.4; y2 = 0.2; y3 = 0.0; y4 = -0.2; y5 = -0.4; y6 = -0.6; y7 = -0.8;


deltas = deltaContour b a

(zp,zn) = partition ((<0).snd.fst) deltas


msg s d = Draw [winTitle s, Draw d]

shOrig :: (Polyline, [Int]) -> Drawing
shOrig (p, os) = Draw [Draw p, color white $ drawThings (polyPts p) (zip [0..] os)]

drawThings pts xs = draws $ zipWith (textF Helvetica10) pts (map ((' ':).show) xs)


drawDelta = Draw . map shDelta
  where
    shDelta ((_,oa),bs) = color c bs
      where
        c | oa < 0 = blue
          | otherwise = red


(abug,bbug) = (Closed {polyPts = [Point {px = -0.20782354230467645, py = -9.313176855867426e-2},Point {px = -0.373691244681037, py = -9.63207857734842e-2},Point {px = -0.4532415606894818, py = -0.13805032936096592},Point {px = -0.9057799447203816, py = -0.25127649349486203},Point {px = -0.5168077114432317, py = -0.14968476446231596},Point {px = -0.4036462677777077, py = -0.12340506652447494}]},Closed {polyPts = [Point {px = -0.5571559071540833, py = 8.839972433634102e-4},Point {px = -0.5287163257598877, py = -5.327197164297104e-2},Point {px = -0.5075118541717529, py = -9.433480352163315e-2},Point {px = -0.5550975799560547, py = -0.10797053575515747},Point {px = -0.5856568813323975, py = -0.11764708906412125},Point {px = -0.565544843673706, py = -0.1406146138906479},Point {px = -0.5531058311462402, py = -0.1528235524892807},Point {px = -0.5343605279922485, py = -0.14698220789432526},Point {px = -0.3986378014087677, py = -0.10871192067861557},Point {px = -0.46634629368782043, py = -4.173432942479849e-3},Point {px = -0.4821145236492157, py = 1.7320431768894196e-2},Point {px = -0.48549777269363403, py = 1.6965093091130257e-2}]})

(a,b) = (g abug2, g bbug2)
  where
    g = transPol (scaling 0.5)

(abug2,bbug2) = (Closed {polyPts = [Point {px = -1.0, py = 1.0},Point {px = 0.5409089027863764, py = 1.0},Point {px = 0.5409089027863764, py = 0.5409089027863764},Point {px = 1.0, py = 0.5409089027863764},Point {px = 1.0, py = -1.0},Point {px = -1.0, py = -1.0}]},Closed {polyPts = [Point {px = 1.0461806328931726, py = -1.0533110935239938},Point {px = 1.0323399253198746, py = -1.039564869887659},Point {px = 1.014900304936005, py = -1.0146528169244553},Point {px = 1.022788829650742, py = -0.9881410452615857},Point {px = 1.028325769585851, py = -0.9070272594380744},Point {px = 1.013516069523226, py = -0.8427087254114509},Point {px = 1.0089564024059414, py = -0.5798712901677733},Point {px = 0.9870527094697498, py = -0.2733846975750315},Point {px = 0.9758625235656957, py = -0.24218880530096729},Point {px = 0.9798684716222997, py = -0.21421990438120792},Point {px = 0.9147137844313992, py = 0.7037264535211735},Point {px = 0.9280212551697442, py = 0.7195437046822099},Point {px = 0.9642019137954511, py = 0.7275975578005307},Point {px = 0.9995092302190751, py = 0.7109367779325695},Point {px = 0.9847181427064869, py = 0.5096699314450556},Point {px = 0.9959174286720239, py = 0.5018075747680507},Point {px = 1.006438363521922, py = 0.553969892877748},Point {px = 1.0095068020938829, py = 0.7771989871151818},Point {px = 0.9637698191989975, py = 0.7746502589316928},Point {px = 0.9585951519432351, py = 0.7597354240620862},Point {px = 0.5127452637307122, py = 0.9259402861627014},Point {px = 0.14575152161925722, py = 0.9828461310422835},Point {px = -5.213923675151695e-2, py = 0.9932031389122915},Point {px = -0.48262032183470605, py = 0.9697420007112659},Point {px = -0.685546653214523, py = 0.9380567636001031},Point {px = -1.0439557011507605, py = 0.832135627298649},Point {px = -1.0792260759988408, py = 0.8110871502490191},Point {px = -1.024067004863906, py = 5.816659649778676e-2},Point {px = -0.9488212085934068, py = -1.048481079123252},Point {px = -0.9106120908786688, py = -1.053108950986663},Point {px = -0.4608589886454261, py = -0.9884059807665415},Point {px = -0.12071759837992606, py = -0.9705722756692597},Point {px = 0.20034888373747242, py = -0.9683383345100647},Point {px = 0.5079046252710107, py = -0.9875190877912031},Point {px = 0.8202258965835505, py = -1.026459240859009},Point {px = 1.0176998187798558, py = -1.0638916665851768}]})


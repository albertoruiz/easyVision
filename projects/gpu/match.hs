import Vision.GUI
import ImagProc hiding (median)
import ImagProc.GPU.SIFT
import Util.Misc(debug, median, degree)
import Vision
import Util.Geometry
import Numeric.LinearAlgebra
import Text.Printf(printf)
import Util.Rotation

getView m = clickKeep "click to set base view" (const id) (g m) Nothing
 
main = do
    let h = testHR 30 0 10
    print h
    print $ fsFromHRot h
    print $ fsFromHRot h !! 2
    --error "stop"
    prepare
    match <- getMatchGPU
    runNT_ camera $ sift grayscale >>> getView match


g match ((x,psx),(s,pss)) = Draw [ Draw (rgb x)
                                 , text (Point 0.9 (-0.65)) info
                                 , color red drmatches ]
  where
    matches' = match 0.7 0.8 psx pss
    matches = map (\[a,b]-> (psx!! a, pss!! b)) matches'
    (h,matchesH) | length matches >= 10 = (compHomog (5/320) matches)
                 | otherwise = (ident 3, [])
    (f,matchesF) | length matches >= 10 = (compFund (5/320) matches)
                 | otherwise = (ident 3, [])
    errH = repro h matchesH
    --matchesOk = if errH < 3 / 320 then matchesH else matchesF
    (colm, matchesOk) = if lh > lm then (red, matchesH) else (green, matchesF)
    drmatches = map (\(p,q) -> Segment p q) matchesOk
    lh = length matchesH
    lf = length matchesF
    lm = length matches
    li = length matchesOk
    info = printf "EH=%.1f F=%d M=%d MH=%d MF=%d O=%d P=%.0fm fHR=%.1f fF=%.1f"
                  (320*errH) (length psx) lm lh lf (lm-li) (1000*proj h) (fsFromHRot h!!2) (estimf)
    (_,estimf,_) = estimateEssential 2 f

compModel meth th matches = (m, map g inls)
  where
    (m,inls) = meth th (f dest) (f orig) where
    (orig,dest) = unzip matches
    f = map (p2l.ipPosition)
    g (p,q) = (l2p p, l2p q)

compHomog = compModel estimateHomographyRansac

compFund = compModel (estimateFundamentalRansac 0.99)


p2l (Point x y) = [x,y]
l2p [x,y] = Point x y

repro h' ms | null ms = 1/0
            | otherwise = pnorm PNorm1 (d-s) / fromIntegral (rows d)
  where
    s = fromRows . map (toVector . fst) $ ms
    d = fromRows . map toVector . (h <|) . map snd $ ms
    h = unsafeFromMatrix h' :: Homography

proj h = (abs h1 + abs h2) / abs h3
  where
    [h1,h2,h3] = toList $ last $ toRows h


fsFromHRot h' = [f1,f2,f3,f4,f5,f6]
  where
    [h11,h12,h13,
     h21,h22,h23,
     h31,h32,h33] = toList $ flatten $ normatdet h'
    f1 = sqrt $ (1-h11**2-h21**2)/h31**2
    f2 = sqrt $ (1-h12**2-h22**2)/h32**2
    f3 = sqrt $ (-h13**2-h23**2)/(-1+h33**2) -- best !?
    f4 = sqrt $ (-h11*h12-h21*h22)/(h31*h32)
    f5 = sqrt $ (-h11*h13-h21*h23)/(h31*h33)
    f6 = sqrt $ (-h12*h13-h22*h23)/(h32*h33)

testHR a3 a2 a1 = k <> r <> inv k
  where
    k = kgen 2.5
    r = rot3(a3*degree) <> rot2(a2*degree) <> rot1(a1*degree)


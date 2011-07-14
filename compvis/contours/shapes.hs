{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

import EasyVision
import Control.Arrow((***),(&&&))
import Graphics.UI.GLUT hiding (Point,Size)
import Data.Colour.Names
import Numeric.LinearAlgebra
import Text.Printf(printf)
import Data.List(minimumBy,sortBy,groupBy)
import Util.Misc(Mat,Vec,norm,degree,diagl,debug,posMax)
import Util.Rotation
import Classifier(Sample)
import Vision
import Util.Options(optionFromFile)

import Control.Monad(when)
import Data.Maybe(isJust)

import Tools(toCanonicAll,toCanonic2,maxFreq,toFun,AlignInfo,digits)

square = Closed $ map (\(a,b)->Point a b) [(0, 0), (0, 0.25), (0, 0.5), (0, 0.75), (0,1), (0.25, 1), (0.5, 1), (0.75, 1), (1, 1), (1, 0.75), (1, 0.5), (1, 0.25), (1, 0), (0.75, 0), (0.5, 0), (0.25,0)]


main = run $ camera  ~> grayscale
--         >>= detectStatic 0.02 1.5 5 grayscale rgb ~> grayscale
         >>= wcontours id ~> (id *** contSel)
         ~>  id *** filter (not . elongated 5) . map shape
         >>= injectPrototypes normalShape (optionFromFile "--catalog" [])
         >>= showCanonical
         ~>  preClassify >>= showAlign
         >>= timeMonitor

shape :: Polyline -> Shape
shape = analyzeShape . (id &&& momentsContour . polyPts)

elongated r Shape { shapeAxes = (l1,l2,_) } = sqrt l2 / sqrt l1 < 1/r 

----------------------------------------------------------------------

injectPrototypes prepro ioprots = shapeCatalog fst (map shapeContour.snd) prepro ioprots (map (shape *** id))

showContours = contourMonitor "contours" fst (lineWidth $= 2 >> setColor' red) snd

showCanonical = contourMonitor "canonical" (fst.fst) (lineWidth $= 3 >> setColor' yellow) (map f . snd . fst)
  where
    f Shape {whiteShape = s, shapeMoments = (ox,oy,_,_,_), kAngles = a:as }  = transPol t s
      where
        t = desp (ox,oy) <> diagl [0.05,0.05,1] <> rot3 a

----------------------------------------------------------------------
type CVec = Vector (Complex Double)

data Shape = Shape { shapeContour  :: Polyline
                   , shapeMoments  :: (Double,Double,Double,Double,Double)
                   , shapeAxes     :: (Double,Double,Double)
                   , shapeWhitener :: Mat
                   , whiteShape    :: Polyline
                   , fourier       :: Int -> Complex Double
                   , featInv       :: Vec
                   , invSimil      :: Vec
                   , kAngles       :: [Double]
                   }



analyzeShape (p,(mx,my,cxx,cyy,cxy)) = Shape {
    shapeContour = p,
    shapeMoments = (mx,my,cxx,cyy,cxy),
    shapeAxes = (l1,l2,phi),
    shapeWhitener = w,
    whiteShape = r,
    fourier = normalizeStart fou,
    featInv = fromList $ map (magnitude.fou) [-10 .. 10],
    invSimil = featNotBad,
    kAngles = as }
  where
    (l1,l2,phi) = eig2x2Dir (cxx,cyy,cxy)
    w = rot3 (27*degree) <> diagl [1/sqrt l1,1/sqrt l2,1] <> rot3 phi <> desp (-mx,-my)
    r = transPol w p
    as = icaAngles r    
    fou = fourierPL r
    fouSimil = magnitude . fourierPL p
    featBad = fromList [ f k / f 1 | k <- [ 2 .. 15 ]]
       where
         f k = fouSimil k + fouSimil (-k)
    featNotBad = fromList [ f k / s | k <- [-15,-14 .. -2] ++ [2..15]]
       where
         f = fouSimil
         s = f 1 + f (-1)
----------------------------------------------------------------------

preClassify ((x,cs),prots) = (x, classifInvar prots cs)

classifInvar :: Sample Shape -> [Shape] -> [[((Double, String), (Shape, Shape))]]
classifInvar prots cs = oks
  where
    theDist = fst.fst
    oks =  map (sortBy (compare `on` theDist) . filter ((<0.2).theDist) . clas) cs
    clas x = map (basicDist x) prots
    basicDist x (y,l) = ((d,l),(x,y))
      where
        d = dist (featInv x) (featInv y)
        --d = dist (invSimil x) (invSimil y)

dist a b = pnorm PNorm2 (a-b)
d2f = fromRational.toRational

----------------------------------------------------------------------

showAlign cam = monitorWheel (0,0,2) "Detected" (mpSize 20) sh cam
  where
    sh k (im, oks) = do
        drawImage' im
        pointCoordinates (size im)
        setColor' white
        text2D 0.9 0.6 $ show (length oks)
        mapM_ (h k) oks

    h 0 [] = return ()
    h 0 (((d,l),(x,p)):ps) = text2D (d2f ox) (d2f oy) (printf "%s (%.0f)" ls (d*100))
      where
        Shape { shapeMoments = (ox,oy,_,_,_) } = x
        ls = l ++ concatMap (snd.fst) ps

----------------------------------------------------------------------

rev (Closed ps) = Closed (reverse ps)
rev (Open ps) = Open (reverse ps)

----------------------------------------------------------------------

--alignment :: Sample (Mat,Vec) -> Polyline -> AlignInfo
alignment prots x = minimumBy (compare `on` fst) $ [basicDist x z p | z <- zs, p <- prots]
  where
    zs = toCanonicAll x
    basicDist x (b,u) ((a,v),l) = (dist u v, (x,b,u,a,v,l))
    dist a b = pnorm PNorm2 (a-b)

classifier (im,cs,prots) = (im, oks)
  where
    oks = filter ((<0.2).fst) (map clas cs) 
    clas = alignment prots

----------------------------------------------------------------------

showAlignment cam = monitorWheel (0,0,2) "Detected" (mpSize 20) sh cam
  where
    sh k (im, oks) = do
        drawImage' im
        pointCoordinates (size im)
        setColor' white
        text2D 0.9 0.6 $ show (length oks)
        mapM_ (h k) oks

    h 0 = f0 . g1
    h 1 = f1 . g1
    h 2 = f2 . g2

    g1 (d, (x,b,u,a,v,l)) = (d,((x, inv b <> a),l))

    f0 (d,((c,m),l)) = do
        text2D (d2f ox) (d2f oy) l
      where
        (ox,oy,_,_,_) = momentsContour (polyPts c)

    f1 (d,((c,m),l)) = do
        setColor' white
        renderPolyline c
        text2D (d2f ox) (d2f up) l -- (printf "%s %.0f" l (d*100))
        setColor' yellow
        renderPolyline $ (transPol m) (Closed [Point (-1) (-1), Point 1 (-1), Point 1 1, Point (-1) 1])
        setColor' red
        renderPolyline $ (transPol m) (Closed [Point 0 0, Point (-2) 0, Point 0 0, Point 0 2])
      where
        (ox,oy,_,_,_) = momentsContour (polyPts c)
        up = 2/400 + maximum (map py (polyPts c))

    g2 (d, (x,b,u,a,v,l)) = (d,x,(b,(u,v)),l)
    f2 (d,x,(mb,(cx,cb)),l) = do
        lineWidth $= 1
        let al = transPol (inv mb) (invFou 100 maxFreq $ toFun cb)
            ob = transPol (inv mb) (invFou 100 maxFreq $ toFun cx)
            corr = zipWith (\a b->[a,b]) (polyPts al) (polyPts ob)
        setColor' yellow
        let aux (Closed ps) = map p2l ps
            p2l (Point x y) = [x,y]
        renderPrimitive Lines $ mapM_ vertex (concat corr)
        lineWidth $= 1
        setColor' red
        renderPolyline al
        setColor' black
        text2D (d2f ox) (d2f up) (printf "%s %.0f" l (d*100))
      where
        (ox,oy,_,_,_) = momentsContour (polyPts x)
        up = 2/400 + maximum (map py (polyPts x))


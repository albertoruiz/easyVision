
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
import Control.Applicative((<$>))
import Data.Maybe(isJust)

import Shapes
import NewTools

import Tools(toCanonicAll,toCanonic2,maxFreq,toFun,AlignInfo,digits)

square = Closed $ map (\(a,b)->Point a b) [(0, 0), (0, 0.25), (0, 0.5), (0, 0.75), (0,1), (0.25, 1), (0.5, 1), (0.75, 1), (1, 1), (1, 0.75), (1, 0.5), (1, 0.25), (1, 0), (0.75, 0), (0.5, 0), (0.25,0)]

catalog = (read <$> readFile "digits.txt") >>= optionFromFile "--catalog"

main = main1

main1 = run $ camera  ~> grayscale
--         >>= detectStatic 0.02 1.5 5 grayscale rgb ~> grayscale
         >>= wcontours id ~> (id *** contSel)
         ~>  id *** filter (not . elongated 5) . map shape
         >>= injectPrototypes boxShape catalog
--         >>= showCanonical
         ~>  matchShapes 0.3 0.25
         >>= showAlign
         >>= timeMonitor

main2 = run $ camera  ~> grayscale
          >>= observe "image" id
          >>= wcontours id ~> (id *** contSel)
          >>= showContours
          >>= timeMonitor

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
        textAt (Point ox oy) l
      where
        (ox,oy,_,_,_) = momentsContour (polyPts c)

    f1 (d,((c,m),l)) = do
        setColor' white
        renderPolyline c
        textAt (Point ox up) l -- (printf "%s %.0f" l (d*100))
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
        textAt (Point ox up) (printf "%s %.0f" l (d*100))
      where
        (ox,oy,_,_,_) = momentsContour (polyPts x)
        up = 2/400 + maximum (map py (polyPts x))


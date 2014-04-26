import Vision.GUI
import Image.Processing
import Contours.Polygons
import Contours
import Image.Processing
import Util.Geometry
import Vision(scaling)
import Text.Printf(printf)

otsuContours x = contours 1000 100 otsu
  where
    otsu = x .<. (fromIntegral (otsuThreshold x))

quadrilaterals = take 1 . polygons 10 5 (4,4)
               . map (smoothPolyline 4)
               . otsuContours

main = run $ arr (grayscale >>> id &&& quadrilaterals) >>> observe "horizon" sh
          
sh (im,qs) = [dr]
  where
    dr = Draw [ Draw simg, (Draw . map (drawContourLabeled blue red white 2 3)) sqs
              , drhoriz ]
    
    h = unsafeFromMatrix $ scaling (1/3) :: Homography
    sqs = map (Closed . (h <|) . polyPts) qs
    simg = warp 192 (size im) (toMatrix h) im
    drhoriz | null qs = Draw ()
            | otherwise = color blue [ Draw [l1,l2,l3,l4]
                                     , color red l_inf'
                                     , pointSz 3 [q1,q2,homog n]
                                     , color red $ Segment (Point 0 0) n
                                     , text (Point 0.9 0) (printf "f ~ %.2f" f)
                                     ]
    Closed [p1,p2,p3,p4] = head sqs
    l1 = gjoin p1 p2
    l2 = gjoin p3 p4
    l3 = gjoin p1 p4
    l4 = gjoin p2 p3
    q1 = meet l1 l2
    q2 = meet l3 l4
    l_inf' = gjoin q1 q2
    n = inhomog $ closestToLine (HPoint 0 0 1) l_inf'
    x1 = distPoints (inhomog q1) n
    x2 = distPoints (inhomog q2) n
    yh = distPoints  (Point 0 0) n
    f = 3* sqrt (x1*x2-yh**2)
    
dirNormal (HLine a b c) = HPoint a b 0

closestToLine p l = gjoin p (dirNormal l) `meet` l


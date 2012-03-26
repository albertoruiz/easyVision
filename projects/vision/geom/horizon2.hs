import Vision.GUI
import ImagProc
import Vision.Apps.Contours
import Util.Geometry
import Vision(scaling)

quadrilaterals = polygons 10 5 (4,4)
               . map (smoothPolyline 4)
               . fst . fst . otsuContours

main = run $ arr (grayscale >>> id &&& quadrilaterals) >>> observe "horizon" sh
          
sh (im,qs) = [dr]
  where
    dr = Draw [ Draw simg, (Draw . map (drawContourLabeled blue red white 2 3)) sqs
              , drhoriz ]
    
    h = unsafeFromMatrix $ scaling (1/3) :: Homography
    sqs = map (Closed . (h <|) . polyPts) qs
    simg = warp 192 (size im) (toMatrix h) im
    drhoriz | null qs = Draw ()
            | otherwise = color blue [l_inf',l1,l2,l3,l4]
    Closed [p1,p2,p3,p4] = head sqs
    l1 = join p1 p2
    l2 = join p3 p4
    l3 = join p1 p4
    l4 = join p2 p3
    l_inf' = (meet l1 l2) `join` (meet l3 l4)


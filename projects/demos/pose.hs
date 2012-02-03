import Vision.GUI
import ImagProc
import Vision.Apps.Contours
import Numeric.LinearAlgebra
import Data.List(sortBy)
import Data.Maybe(catMaybes)
import Vision(cameraFromHomogZ0,estimateHomography,ht) 
import Util.Misc(rotateLeft)
import Data.Function(on)

darkContours = (id &&& (otsuContours >>> (map (smoothPolyline 4) . fst .fst)))

main = run $ arr grayscale
          >>> arr darkContours
          >>> sMonitor "camera pose" shinfo
          
shinfo _ (im,cs) = [ augmented im ps
                   , drawsP (take 1 ps) ]
  where
    drawsP xs = Draw [ Draw im, (Draw . map (drawContourLabeled blue red white 2 3)) xs ]
    ps = polygons 10 5 (length ref, length ref) cs


augmented img ps = case rectifiers ps of
        []    -> Draw img
        cam:_ -> viewPoint cam (Just img) thing
  where
    thing = Raw $ block 1 1 1

       
rectifiers pss = catMaybes
               . map (cameraFromHomogZ0 Nothing)
               . map fst . sortBy (compare `on` repro)
               . map (flip estimateHomography ref &&& id)
               $ rots
  where
   p2l (Point x y) = [x,y]
   lps = map (map p2l . polyPts) pss
   rots1 c = map (flip rotateLeft c) [0 .. length c -1]
   rots = concatMap rots1 lps
   repro (h,ps) = pnorm PNorm1 (fromLists (ht h ref) - fromLists ps)

ref = [[0,0]
      ,[1,0]
      ,[1,-1]
      ,[-1,-1]
      ,[-1,1]
      ,[0,1]]


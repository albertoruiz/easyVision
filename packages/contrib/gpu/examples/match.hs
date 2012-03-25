import Vision.GUI
import ImagProc
import ImagProc.GPU.SIFT
import Util.Misc(debug)

getView m = clickKeep "click to set base view" (const id) (g m) Nothing
 
main = do
    prepare
    match <- getMatchGPU
    runNT_ camera $ sift grayscale >>> getView match


g match ((x,psx),(s,pss)) = Draw [ Draw (rgb x)
                                 , text (Point 0 0) (show (length psx, length matches))
                                 , color red matches ]
  where
    matches' = match 0.7 0.8 psx pss
    matches = map (\[a,b]-> (Segment (ipPosition (psx!! a)) (ipPosition(pss!! b)))) matches'

